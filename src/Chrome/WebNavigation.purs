module Chrome.WebNavigation where

import MasonPrelude
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Debug as Debug

type Details
  = { frameId :: Int
    , parentFrameId :: Int
    , tabId :: Int
    , timeStamp :: Number
    , url :: String
    }

onBeforeNavigate :: Chrome Details
onBeforeNavigate = Chrome.wrapListener "webNavigation" "onBeforeNavigate"
