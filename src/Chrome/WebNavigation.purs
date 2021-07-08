module Chrome.WebNavigation where

import MasonPrelude
import Chrome.Wrap (Chrome, ChromeEvent)
import Chrome.Wrap as Chrome
import Control.Monad.Except (throwError)
import Foreign (ForeignError(..))
import Simple.JSON as Json
import Simple.JSON (class ReadForeign)

type BaseDetails r
  = { frameId :: Int
    , parentFrameId :: Int
    , tabId :: Int
    , timeStamp :: Number
    , url :: String
    | r
    }

type BeforeNavigateDetails
  = BaseDetails ()

onBeforeNavigate :: Chrome BeforeNavigateDetails
onBeforeNavigate = Chrome.wrapListener "webNavigation" "onBeforeNavigate"

onBeforeNavigateE :: ChromeEvent BeforeNavigateDetails
onBeforeNavigateE = Chrome.wrapEvent "webNavigation" "onBeforeNavigate"

data TransitionType
  = Link
  | Typed
  | AutoBookmark
  | AutoSubframe
  | ManualSubframe
  | Generated
  | StartPage
  | FormSubmit
  | Reload
  | Keyword
  | KeywordGenerated

derive instance eqTransitionType :: Eq TransitionType

instance readForeignTransitionType :: ReadForeign TransitionType where
  readImpl =
    Json.read'
    >=> case _ of
          "link" -> pure Link
          "typed" -> pure Typed
          "auto_bookmark" -> pure AutoBookmark
          "auto_subframe" -> pure AutoSubframe
          "manual_subframe" -> pure ManualSubframe
          "generated" -> pure Generated
          "start_page" -> pure StartPage
          "form_submit" -> pure FormSubmit
          "reload" -> pure Reload
          "keyword" -> pure Keyword
          "keyword_generated" -> pure KeywordGenerated

          str ->
            throwError
            $ pure $ TypeMismatch ("one of: " <> intercalate ", " transitionTypes) str

transitionTypes :: Array String
transitionTypes =
  [ "link"
  , "typed"
  , "auto_bookmark"
  , "auto_subframe"
  , "manual_subframe"
  , "generated"
  , "start_page"
  , "form_submit"
  , "reload"
  , "keyword"
  , "keyword_generated"
  ]

data TransitionQualifier
  = ClientRedirect
  | ServerRedirect
  | ForwardBack
  | FromAddressBar

derive instance eqTransitionQualifier :: Eq TransitionQualifier

instance readForeignTransitionQualifier :: ReadForeign TransitionQualifier where
  readImpl =
    Json.read'
    >=> case _ of
          "client_redirect" -> pure ClientRedirect
          "server_redirect" -> pure ServerRedirect
          "forward_back" -> pure ForwardBack
          "from_address_bar" -> pure FromAddressBar

          str ->
            throwError
            $ pure $ TypeMismatch ("one of: " <> intercalate ", " transitionQualifiers) str

transitionQualifiers :: Array String
transitionQualifiers =
  [ "client_redirect"
  , "server_redirect"
  , "forward_back"
  , "from_address_bar"
  ]

type CommittedDetails
  = BaseDetails
      ( processId :: Int
      , transitionQualifiers :: Array TransitionQualifier
      , transitionType :: TransitionType
      )

onCommitted :: Chrome CommittedDetails
onCommitted = Chrome.wrapListener "webNavigation" "onCommitted"

onCommittedE :: ChromeEvent CommittedDetails
onCommittedE = Chrome.wrapEvent "webNavigation" "onCommitted"
