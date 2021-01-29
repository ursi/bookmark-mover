module Chrome.Tabs where

import MasonPrelude
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Control.Monad.Except (throwError)
import Debug as Debug
import Foreign (ForeignError(..))
import Simple.JSON as Json
import Simple.JSON (class ReadForeign, class WriteForeign)

type Tab
  = { active :: Boolean
    , autoDiscardable :: Boolean
    , discarded :: Boolean
    , pinned :: Boolean
    , audible :: Maybe Boolean
    , favIconUrl :: Maybe String
    , groupId :: Maybe Int
    , height :: Maybe Int
    , highlighted :: Maybe Boolean
    , id :: Maybe Int
    , incognito :: Maybe Boolean
    , index :: Maybe Int
    , mutedInfo :: Maybe MutedInfo
    , openerTabId :: Maybe Int
    , pendingUrl :: Maybe String
    , sessionId :: Maybe String
    , status :: Maybe TabStatus
    , title :: Maybe String
    , url :: Maybe String
    , width :: Maybe Int
    , windowId :: Maybe Int
    }

type MutedInfo
  = { extensionId :: Maybe String
    , muted :: Boolean
    , reason :: Maybe MutedInfoReason
    }

data MutedInfoReason
  = User
  | Capture
  | Extension

instance readForeignMutedInfoReason :: ReadForeign MutedInfoReason where
  readImpl =
    Json.read'
      >=> case _ of
          "user" -> pure User
          "capture" -> pure Capture
          "extension" -> pure Extension
          str ->
            throwError $ pure
              $ TypeMismatch "one of: user, capture, extension" str

data TabStatus
  = Unloaded
  | Loading
  | Complete

derive instance eqTabStatus :: Eq TabStatus

instance readForeignTabStatus :: ReadForeign TabStatus where
  readImpl =
    Json.read'
      >=> case _ of
          "unloaded" -> pure Unloaded
          "loading" -> pure Loading
          "complete" -> pure Complete
          str ->
            throwError $ pure
              $ TypeMismatch "one of: unloaded, loading, complete" str

instance writeForeignTabStatus :: WriteForeign TabStatus where
  writeImpl = case _ of
    Unloaded -> Json.write "unloaded"
    Loading -> Json.write "loading"
    Complete -> Json.write "complete"

type ChangeInfo
  = { audible :: Maybe Boolean
    , autoDiscardable :: Maybe Boolean
    , discarded :: Maybe Boolean
    , favIconUrl :: Maybe String
    , groupId :: Maybe Int
    , pinned :: Maybe Boolean
    , status :: Maybe TabStatus
    , title :: Maybe String
    , url :: Maybe String
    }

type OnUpdated
  = { tabId :: Int
    , changeInfo :: ChangeInfo
    , tab :: Tab
    }

onUpdated :: Chrome OnUpdated
onUpdated =
  Chrome.wrapListener3 "tabs" "onUpdated"
    { tabId: _, changeInfo: _, tab: _ }

onCreated :: Chrome Tab
onCreated = Chrome.wrapListener "tabs" "onCreated"

type QueryInfo
  = { active :: Maybe Boolean
    , audible :: Maybe Boolean
    , autoDiscardable :: Maybe Boolean
    , currentWindow :: Maybe Boolean
    , discarded :: Maybe Boolean
    , groupId :: Maybe Int
    , highlighted :: Maybe Boolean
    , index :: Maybe Int
    , lastFocusedWindow :: Maybe Boolean
    , muted :: Maybe Boolean
    , pinned :: Maybe Boolean
    , status :: Maybe TabStatus
    , title :: Maybe String
    , url :: Maybe (Array String)
    , windowId :: Maybe Number
    , windowType :: Maybe WindowType
    }

data WindowType
  = Normal
  | Popup
  | Panel
  | App
  | Devtools

instance readForeignWindowType :: ReadForeign WindowType where
  readImpl =
    Json.read'
      >=> case _ of
          "normal" -> pure Normal
          "popup" -> pure Popup
          "panel" -> pure Panel
          "app" -> pure App
          "devtools" -> pure Devtools
          str ->
            throwError $ pure
              $ TypeMismatch "one of: normal, popup, panel, app, devtools" str

instance writeForeignWindowType :: WriteForeign WindowType where
  writeImpl = case _ of
    Normal -> Json.write "normal"
    Popup -> Json.write "popup"
    Panel -> Json.write "panel"
    App -> Json.write "app"
    Devtools -> Json.write "devtools"

defaultQuery :: QueryInfo
defaultQuery =
  { active: Nothing
  , audible: Nothing
  , autoDiscardable: Nothing
  , currentWindow: Nothing
  , discarded: Nothing
  , groupId: Nothing
  , highlighted: Nothing
  , index: Nothing
  , lastFocusedWindow: Nothing
  , muted: Nothing
  , pinned: Nothing
  , status: Nothing
  , title: Nothing
  , url: Nothing
  , windowId: Nothing
  , windowType: Nothing
  }

query :: QueryInfo -> Chrome (Array Tab)
query q = Chrome.wrapApi "tabs" "query" [ Json.write q ]
