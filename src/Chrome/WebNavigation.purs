module Chrome.WebNavigation where

import MasonPrelude
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Debug as Debug

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
