module Chrome.Tabs where

import MasonPrelude
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Data.Argonaut
  ( class DecodeJson
  , JsonDecodeError(..)
  , (.:)
  , (.:!)
  , decodeJson
  )
import Data.Argonaut as Arg
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

instance decodeJsonTabStatus :: DecodeJson TabStatus where
  decodeJson json =
    Arg.toString json
      # maybe (Left $ TypeMismatch "I'm looking for a string here") case _ of
          "unloaded" -> Right Unloaded
          "loading" -> Right Loading
          "complete" -> Right Complete
          _ -> Left $ UnexpectedValue json

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
