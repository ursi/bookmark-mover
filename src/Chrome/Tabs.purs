module Chrome.Tabs where

import MasonPrelude
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , JsonDecodeError(..)
  , (.:)
  , (.:!)
  , (:=?)
  , (~>?)
  , decodeJson
  , jsonEmptyObject
  )
import Data.Argonaut as Arg
import Data.Argonaut.Encode.Encoders (encodeString)
import Data.Newtype (class Newtype)
import Debug as Debug

newtype Tab
  = Tab
  { active :: Boolean
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

derive instance newtypeTab :: Newtype Tab _

instance decodeJsonTab :: DecodeJson Tab where
  decodeJson json =
    Tab
      <$> do
          obj <- decodeJson json
          active <- obj .: "active"
          autoDiscardable <- obj .: "autoDiscardable"
          discarded <- obj .: "discarded"
          pinned <- obj .: "pinned"
          audible <- obj .:! "audible"
          favIconUrl <- obj .:! "favIconUrl"
          groupId <- obj .:! "groupId"
          height <- obj .:! "height"
          highlighted <- obj .:! "highlighted"
          id <- obj .:! "id"
          incognito <- obj .:! "incognito"
          index <- obj .:! "index"
          mutedInfo <- obj .:! "mutedInfo"
          openerTabId <- obj .:! "openerTabId"
          pendingUrl <- obj .:! "pendingUrl"
          sessionId <- obj .:! "sessionId"
          status <- obj .:! "status"
          title <- obj .:! "title"
          url <- obj .:! "url"
          width <- obj .:! "width"
          windowId <- obj .:! "windowId"
          pure
            { active
            , autoDiscardable
            , discarded
            , pinned
            , audible
            , favIconUrl
            , groupId
            , height
            , highlighted
            , id
            , incognito
            , index
            , mutedInfo
            , openerTabId
            , pendingUrl
            , sessionId
            , status
            , title
            , url
            , width
            , windowId
            }

newtype MutedInfo
  = MutedInfo
  { extensionId :: Maybe String
  , muted :: Boolean
  , reason :: Maybe MutedInfoReason
  }

derive instance newtypeMutedInfo :: Newtype MutedInfo _

instance decodeJsonMutedInfo :: DecodeJson MutedInfo where
  decodeJson json =
    MutedInfo
      <$> do
          obj <- decodeJson json
          extensionId <- obj .:! "extensionId"
          muted <- obj .: "muted"
          reason <- obj .:! "reason"
          pure { extensionId, muted, reason }

data MutedInfoReason
  = User
  | Capture
  | Extension

instance decodeJsonMutedInfoReason :: DecodeJson MutedInfoReason where
  decodeJson json =
    Arg.toString json
      # maybe (Left $ TypeMismatch "I'm looking for a string here") case _ of
          "user" -> Right User
          "capture" -> Right Capture
          "extension" -> Right Extension
          _ -> Left $ UnexpectedValue json

data TabStatus
  = Unloaded
  | Loading
  | Complete

derive instance eqTabStatus :: Eq TabStatus

instance decodeJsonTabStatus :: DecodeJson TabStatus where
  decodeJson json =
    Arg.toString json
      # maybe (Left $ TypeMismatch "I'm looking for a string here") case _ of
          "unloaded" -> Right Unloaded
          "loading" -> Right Loading
          "complete" -> Right Complete
          _ -> Left $ UnexpectedValue json

instance encodeJsonTabStatus :: EncodeJson TabStatus where
  encodeJson = case _ of
    Unloaded -> encodeString "unloaded"
    Loading -> encodeString "loading"
    Complete -> encodeString "complete"

newtype ChangeInfo
  = ChangeInfo
  { audible :: Maybe Boolean
  , autoDiscardable :: Maybe Boolean
  , discarded :: Maybe Boolean
  , favIconUrl :: Maybe String
  , groupId :: Maybe Int
  , pinned :: Maybe Boolean
  , status :: Maybe TabStatus
  , title :: Maybe String
  , url :: Maybe String
  }

derive instance newtypeChangeInfo :: Newtype ChangeInfo _

instance decodeJsonChangeInfo :: DecodeJson ChangeInfo where
  decodeJson json =
    ChangeInfo
      <$> do
          obj <- decodeJson json
          audible <- obj .:! "audible"
          autoDiscardable <- obj .:! "autoDiscardable"
          discarded <- obj .:! "discarded"
          favIconUrl <- obj .:! "favIconUrl"
          groupId <- obj .:! "groupId"
          pinned <- obj .:! "pinned"
          status <- obj .:! "status"
          title <- obj .:! "title"
          url <- obj .:! "url"
          pure
            { audible
            , autoDiscardable
            , discarded
            , favIconUrl
            , groupId
            , pinned
            , status
            , title
            , url
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

instance decodeJsonWindowType :: DecodeJson WindowType where
  decodeJson json =
    Arg.toString json
      # maybe (Left $ TypeMismatch "I'm looking for a string here") case _ of
          "normal" -> Right Normal
          "popup" -> Right Popup
          "panel" -> Right Panel
          "app" -> Right App
          "devtools" -> Right Devtools
          _ -> Left $ UnexpectedValue json

instance encodeJsonWindowType :: EncodeJson WindowType where
  encodeJson = case _ of
    Normal -> encodeString "normal"
    Popup -> encodeString "popup"
    Panel -> encodeString "panel"
    App -> encodeString "app"
    Devtools -> encodeString "devtools"

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
query q =
  Chrome.wrapApi "tabs" "query"
    [ "active" :=? q.active
        ~>? ("audible" :=? q.audible)
        ~>? ("autoDiscardable" :=? q.autoDiscardable)
        ~>? ("currentWindow" :=? q.currentWindow)
        ~>? ("discarded" :=? q.discarded)
        ~>? ("groupId" :=? q.groupId)
        ~>? ("highlighted" :=? q.highlighted)
        ~>? ("index" :=? q.index)
        ~>? ("lastFocusedWindow" :=? q.lastFocusedWindow)
        ~>? ("muted" :=? q.muted)
        ~>? ("pinned" :=? q.pinned)
        ~>? ("status" :=? q.status)
        ~>? ("title" :=? q.title)
        ~>? ("url" :=? q.url)
        ~>? ("windowId" :=? q.windowId)
        ~>? ("windowType" :=? q.windowType)
        ~>? jsonEmptyObject
    ]
