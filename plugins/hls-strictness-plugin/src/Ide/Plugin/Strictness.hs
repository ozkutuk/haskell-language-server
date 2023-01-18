{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Ide.Plugin.Strictness
  ( descriptor
  -- , Log
  ) where

import           Control.Exception                     (evaluate, try)
import           Control.Lens                          ((^.))
import           Control.Monad                         ((<=<))
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Trans.Except            (ExceptT)
import           Data.Generics                         (GenericQ, listify)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (catMaybes, fromJust,
                                                        mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Debug.Trace                           (traceM)
import           Development.IDE                       (IdeState,
                                                        NormalizedFilePath,
                                                        Pretty (..), Range (..),
                                                        Recorder (..), Rules,
                                                        WithPriority (..),
                                                        srcSpanToRange)
import           Development.IDE.Core.Rules            (runAction)
import           Development.IDE.Core.RuleTypes        (GenerateCore (..),
                                                        GetModIface (..),
                                                        HiFileResult (..))
import           Development.IDE.Core.Shake            (define, use)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.GHC.Compat            (IdInfo)
import           Development.IDE.GHC.Compat.Core       (Id, IfaceDecl (..),
                                                        IfaceInfoItem (..),
                                                        ModGuts (..), Name,
                                                        StrictSig, getName,
                                                        hs_valds, idInfo,
                                                        mi_decls, nameSrcSpan,
                                                        strictnessInfo)
import           Development.IDE.GHC.Compat.Outputable (Outputable (..))
import           Development.IDE.GHC.Compat.Util       (GhcException)
import           Development.IDE.GHC.Util              (printOutputable)
import           Development.IDE.Graph                 (RuleResult)
import           Development.IDE.Graph.Classes         (Hashable, NFData (rnf))
import           GHC.Generics                          (Generic)
import           GHC.Iface.Load                        (pprModIface)
import           Ide.Plugin.RangeMap                   (RangeMap)
import qualified Ide.Plugin.RangeMap                   as RangeMap
import           Ide.PluginUtils                       (getNormalizedFilePath,
                                                        handleMaybeM,
                                                        pluginResponse)
import           Ide.Types                             (PluginDescriptor (..),
                                                        PluginId (..),
                                                        PluginMethodHandler,
                                                        defaultPluginDescriptor,
                                                        mkPluginHandler)
import           Language.LSP.Types                    (CodeAction (..),
                                                        CodeActionKind (..),
                                                        CodeActionParams (..),
                                                        Command (..), List (..),
                                                        Method (..),
                                                        SMethod (..),
                                                        type (|?) (InR))
import qualified Language.LSP.Types.Lens               as L



data Log
  = LogShake Shake.Log

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  , pluginRules = collectDmdSigsRule
  }

codeActionProvider :: PluginMethodHandler IdeState 'TextDocumentCodeAction
codeActionProvider ideState pId (CodeActionParams _ _ docId range _) = pluginResponse $ do
  nfp <- getNormalizedFilePath (docId ^. L.uri)
  -- pragma <- getFirstPragma pId ideState nfp
  CDSR result <- collectDmdSigs ideState nfp
  -- TODO(ozkutuk): should construct rangemap as part of the rule
  let f = \(a, b) -> (,(a, b)) <$> nameToRange a
  -- let f = \(a, b) -> (,(a, b)) <$> nameToRange a
  -- TODO(ozkutuk): unnecessary fromList-toList conversion
  let sigMap = RangeMap.fromList' $ mapMaybe f $ filter (not . nullSig . snd) $ Map.toList result
  let actions = map mkCodeAction (RangeMap.filterByRange range sigMap)
  pure $ List actions

  where
    mkCodeAction :: (Name, RenderedDmdSig) -> Command |? CodeAction
    mkCodeAction sig = InR CodeAction
      { _title = mkCodeActionTitle sig
      , _kind = Nothing
      , _diagnostics = Nothing
      , _isPreferred = Nothing
      , _disabled = Nothing
      , _edit = Nothing
      , _command = Nothing
      , _xdata = Nothing
      }

    mkCodeActionTitle :: (Name, RenderedDmdSig) -> Text
    mkCodeActionTitle (nm, RenderedDmdSig sig) =
      "NAME: " <> printOutputable nm <> "    STRICTNESS: " <> sig

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

