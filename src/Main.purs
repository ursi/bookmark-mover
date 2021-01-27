module Main where

import MasonPrelude
import Chrome.Bookmarks (BookmarkTreeNode(..))
import Chrome.Bookmarks as Bookmarks
import Chrome.Tabs (ChangeInfo(..))
import Chrome.Tabs as Tabs
import Chrome.Wrap (Chrome)
import Data.List ((:))
import Control.Monad.Except (runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (unwrap)
import Debug as Debug
import Effect.Aff (launchAff_)
import Foreign.Object as FO

bookmarksBarId :: String
bookmarksBarId = "1"

main :: Effect Unit
main =
  launchAff_
    $ runExceptT do
        bookmarks <-
          Bookmarks.getSubTree bookmarksBarId
            <#> maybe FO.empty Bookmarks.toUrlObj
        forever do
          { changeInfo: (ChangeInfo c) } <- Tabs.onUpdated
          runMaybeT do
              bookmark <- MaybeT $ pure $ c.url >>= FO.lookup ~$ bookmarks
              lift
                $ (getLineage bookmark)
                >>= traverse_
                    ( \id ->
                        Bookmarks.move id
                          { index: Just 0
                          , parentId: Nothing
                          }
                    )

getLineage :: BookmarkTreeNode -> Chrome (List String)
getLineage btn = go (pure $ (unwrap btn).id) btn
  where
  go :: List String -> BookmarkTreeNode -> Chrome (List String)
  go acc (BookmarkTreeNode btn') =
    runMaybeT
      ( do
          parentId <- MaybeT $ pure $ btn'.parentId
          if parentId == bookmarksBarId then
            pure acc
          else
            (MaybeT $ Bookmarks.getOne parentId)
              >>= (lift <. go (parentId : acc))
      )
      <#> fromMaybe acc

-- case c.url of
--   Just url -> logShow $ FO.lookup url bookmarks
--   Nothing -> pure unit
