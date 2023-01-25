{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
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
                                                        cmapWithPrio,
                                                        srcSpanToRange)
import           Development.IDE.Core.Rules            (runAction)
import           Development.IDE.Core.RuleTypes        (GenerateCore (..),
                                                        GetModSummary (..),
                                                        msrModSummary)
import           Development.IDE.Core.Shake            (define, use)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.GHC.Compat            (IdInfo)
import           Development.IDE.GHC.Compat.Core       (DynFlags, Id,
                                                        ModGuts (..),
                                                        ModSummary (..), Name,
                                                        StrictSig, getName,
                                                        idInfo, nameSrcSpan,
                                                        strictnessInfo)
import           Development.IDE.GHC.Compat.Outputable (Outputable (..))
import           Development.IDE.GHC.Compat.Util       (GhcException)
import           Development.IDE.GHC.Util              (printOutputable)
import           Development.IDE.Graph                 (RuleResult)
import           Development.IDE.Graph.Classes         (Hashable, NFData)
import           Development.IDE.Types.Logger          (Priority (..), logWith)
import           GHC.Driver.Session                    (optLevel)
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
  | LogPluginDisabled
  | LogPluginEnabled

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog
    LogPluginDisabled -> "Strictness analysis disabled"
    LogPluginEnabled  -> "Strictness analysis enabled"

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentHover (hoverProvider recorder)
  , pluginRules = collectDmdSigsRule *> checkDmdAnalRule
  }

hoverProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'TextDocumentHover
hoverProvider recorder ideState pId (HoverParams docId pos _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  pluginEnabled <- checkDmdAnal ideState nfp
  case pluginEnabled of
    DmdAnalDisabled -> do
      logWith recorder Debug LogPluginDisabled
      pure Nothing
    DmdAnalEnabled -> do
      logWith recorder Debug LogPluginEnabled
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

collectDmdSigsRule :: Rules ()
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

data CheckDmdAnal = CheckDmdAnal
                  deriving (Eq, Show, Generic)

instance Hashable CheckDmdAnal
instance NFData CheckDmdAnal

data DmdAnalEnabled
  = DmdAnalEnabled
  | DmdAnalDisabled
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

type instance RuleResult CheckDmdAnal = DmdAnalEnabled

-- We are taking a simplistic approach and assuming the demand
-- analysis is not explicitly disabled if the optimization is
-- explicitly enabled.
dmdAnalEnabled :: DynFlags -> DmdAnalEnabled
dmdAnalEnabled df =
  if optLevel df >= 1
    then DmdAnalEnabled
    else DmdAnalDisabled

checkDmdAnalRule ::  Rules ()
checkDmdAnalRule = define mempty $ \CheckDmdAnal nfp ->
   use GetModSummary nfp >>= \case
     Nothing -> pure ([], Nothing)
     Just ms -> do
       let result = dmdAnalEnabled $ ms_hspp_opts $ msrModSummary ms
       pure ([], Just result)

checkDmdAnal :: MonadIO m => IdeState -> NormalizedFilePath -> ExceptT String m DmdAnalEnabled
checkDmdAnal ideState =
  handleMaybeM "Unable to check optimization level"
    . liftIO
    . runAction "Strictness" ideState
    . use CheckDmdAnal
