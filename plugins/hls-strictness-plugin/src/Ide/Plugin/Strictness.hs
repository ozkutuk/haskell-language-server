{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}


module Ide.Plugin.Strictness
  ( descriptor
  -- , Log
  ) where

import           Control.Exception                     (evaluate, try)
import           Control.Lens                          ((^.))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Trans.Except            (ExceptT)
import           Data.Foldable                         (fold)
import           Data.Generics                         (GenericQ, listify)
import           Data.List                             (intersperse)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE                       (IdeState,
                                                        NormalizedFilePath,
                                                        Pretty (..), Range (..),
                                                        Recorder (..), Rules,
                                                        WithPriority (..),
                                                        srcSpanToRange)
import           Development.IDE.Core.Rules            (runAction)
import           Development.IDE.Core.RuleTypes        (GenerateCore (..))
import           Development.IDE.Core.Shake            (define, use)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.GHC.Compat            (IdInfo)
import           Development.IDE.GHC.Compat.Core       (Id, ModGuts (..), Name,
                                                        StrictSig, getName,
                                                        idInfo, nameSrcSpan,
                                                        strictnessInfo)
import           Development.IDE.GHC.Compat.Outputable (Outputable (..))
import           Development.IDE.GHC.Compat.Util       (GhcException)
import           Development.IDE.GHC.Util              (printOutputable)
import           Development.IDE.Graph                 (RuleResult)
import           Development.IDE.Graph.Classes         (Hashable, NFData)
import           GHC.Generics                          (Generic)
import qualified Ide.Plugin.RangeMap                   as RangeMap
import           Ide.PluginUtils                       (getNormalizedFilePath,
                                                        handleMaybeM,
                                                        pluginResponse)
import           Ide.Types                             (PluginDescriptor (..),
                                                        PluginId (..),
                                                        PluginMethodHandler,
                                                        defaultPluginDescriptor,
                                                        mkPluginHandler)
import           Language.LSP.Types                    (Hover (..),
                                                        HoverContents (..),
                                                        HoverParams (..),
                                                        Method (..),
                                                        SMethod (..),
                                                        unmarkedUpContent)
import qualified Language.LSP.Types.Lens               as L



data Log
  = LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentHover hoverProvider
  , pluginRules = collectDmdSigsRule
  }

hoverProvider :: PluginMethodHandler IdeState 'TextDocumentHover
hoverProvider ideState pId (HoverParams docId pos _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  CDSR result <- collectDmdSigs ideState nfp
  -- TODO(ozkutuk): should construct rangemap as part of the rule
  let f (a, b) = (,(a, b)) <$> nameToRange a
  -- TODO(ozkutuk): unnecessary fromList-toList conversion
  let sigMap = RangeMap.fromList' $ mapMaybe f $ filter (not . nullSig . snd) $ Map.toList result
  let hover = mkHover (RangeMap.filterByPosition pos sigMap)
  pure hover

  where
    mkHover :: [(Name, RenderedDmdSig)] -> Maybe Hover
    mkHover [] = Nothing
    -- TODO(ozkutuk): This case distinction between single vs multiple
    -- signatures is only temporary until we decide whether we want multiple
    -- sigs at all
    mkHover [x] = Just $ Hover (mkSingle True x) Nothing
    -- Separate multiple signatures with newlines
    mkHover xs = Just $ Hover (fold . intersperse (toHoverContents "\n") . map (mkSingle False) $ xs) Nothing

    mkSingle :: Bool -> (Name, RenderedDmdSig) -> HoverContents
    mkSingle single p = toHoverContents $ mkSingleText p single

    toHoverContents :: Text -> HoverContents
    toHoverContents = HoverContents . unmarkedUpContent

    -- Bool determines if we want to disambiguate the signature by providing
    -- the corresponding name as well
    mkSingleText :: (Name, RenderedDmdSig) -> Bool -> Text
    mkSingleText (_, RenderedDmdSig sig) True = "Strictness: " <> sig
    mkSingleText (nm, RenderedDmdSig sig) False =
      "Strictness (" <> printOutputable nm <> "): " <> sig

collectDmdSigsRule ::  Rules ()
collectDmdSigsRule = define mempty $ \CollectDmdSigs nfp ->
   use GenerateCore nfp >>= \case
     Nothing -> pure ([], Nothing)
     Just modGuts -> do
       sigs <- liftIO $ getDmdSigs modGuts
       let result = CDSR $ fmap renderDmdSig $ Map.fromList sigs
       -- traceM $ T.unpack $ "DMDSIGSNEW " <> printOutputable (dmdSigs result)

       pure ([], Just result)

nameToRange :: Name -> Maybe Range
nameToRange = srcSpanToRange . nameSrcSpan

getDmdSigs :: ModGuts -> IO [(Name, StrictSig)]
getDmdSigs (mg_binds -> prg) = catMaybes <$> traverse extractSigFromId (collectIds prg)

extractSigFromId :: Id -> IO (Maybe (Name, StrictSig))
extractSigFromId id' = do
  let name = getName id'
  mSig <- fmap strictnessInfo <$> idInfo'
  pure $ (name,) <$> mSig
  where
    idInfo' :: IO (Maybe IdInfo)
    idInfo' = hush <$> try @GhcException (evaluate (idInfo id'))

    hush :: Either e a -> Maybe a
    hush (Right a) = Just a
    hush (Left _)  = Nothing

collectIds :: GenericQ [Id]
collectIds = listify (const True)

collectDmdSigs :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m CollectDmdSigsResult
collectDmdSigs ideState =
  handleMaybeM "Unable to collect demand signatures"
    . liftIO
    . runAction "Strictness" ideState
    . use CollectDmdSigs

data CollectDmdSigs = CollectDmdSigs
                    deriving (Eq, Show, Generic)

instance Hashable CollectDmdSigs
instance NFData CollectDmdSigs

newtype RenderedDmdSig = RenderedDmdSig Text
  deriving newtype (Show, NFData)

nullSig :: RenderedDmdSig -> Bool
nullSig (RenderedDmdSig s) = T.null s

instance Outputable RenderedDmdSig where
  ppr (RenderedDmdSig s) = ppr (T.unpack s)

renderDmdSig :: StrictSig -> RenderedDmdSig
renderDmdSig = RenderedDmdSig . printOutputable

newtype CollectDmdSigsResult = CDSR { dmdSigs :: Map Name RenderedDmdSig }
  deriving stock (Generic)
  deriving newtype (NFData)

instance Show CollectDmdSigsResult where
  show _ = "<CollectDmdSigsResult>"

type instance RuleResult CollectDmdSigs = CollectDmdSigsResult

