module Main where

import MasonPrelude
import Chrome.Bookmarks (BookmarkTreeNode(..))
import Chrome.Bookmarks as Bookmarks
import Chrome.Tabs as Tabs
import Chrome.WebNavigation as WebNav
import Chrome.Wrap (Chrome)
import Data.List ((:))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (isJust)
import Data.Newtype (unwrap)
import Debug as Debug
import Effect.Aff (launchAff_)
import Foreign.Object (Object)
import Foreign.Object as Obj

bookmarksBarId :: String
bookmarksBarId = "1"

main :: Effect Unit
main =
  launchAff_
    $ ( case _ of
          Left e -> logShow e
          Right _ -> pure unit
          =<< ( runExceptT
                $ getBookmakrs
                >>= runStateT
                    ( forever do
                        bookmarks <- get
                        event <-
                          lift
                            $ Debug.log
                            <$> first
                                [ Navigated <$> lift2 Tuple WebNav.onBeforeNavigate Tabs.onUpdated
                                -- ^^ don't use parLift2 because the onUpdate listener gets called multiple times ^^ also the comment is down here because purty sux
                                , BookmarkCreated <$> Bookmarks.onCreated
                                , BookmarkMoved <$> Bookmarks.onMoved
                                , BookmarkChanged <$> Bookmarks.onChanged
                                ]
                        case event of
                          Navigated ({ url } /\ { changeInfo: c }) -> do
                            when (isJust c.url || c.status == Nothing)
                              $ ( lift
                                    $ runMaybeT do
                                        bookmark <- MaybeT $ pure $ Obj.lookup url bookmarks
                                        lift
                                          $ (getLineage bookmark)
                                          >>= traverse_
                                              ( \id ->
                                                  runMaybeT do
                                                    index <- (MaybeT $ Bookmarks.getOne id) <#> unwrap <#> _.index
                                                    void $ lift
                                                      $ Bookmarks.move id
                                                          { index: index <#> (_ / 2)
                                                          , parentId: Nothing
                                                          }
                                                    <#> fromMaybe unit
                                              )
                                )
                              >>= case _ of
                                  Just _ -> lift getBookmakrs >>= put
                                  Nothing -> pure unit
                          _ -> lift getBookmakrs >>= put
                    )
            )
      )

data Event
  = Navigated (WebNav.BeforeNavigateDetails /\ Tabs.OnUpdated)
  | BookmarkCreated { id :: String, bookmark :: BookmarkTreeNode }
  | BookmarkMoved { id :: String, moveInfo :: Bookmarks.MoveInfo }
  | BookmarkChanged { id :: String, changeInfo :: Bookmarks.ChangeInfo }

getBookmakrs :: Chrome (Object BookmarkTreeNode)
getBookmakrs =
  Bookmarks.getSubTree bookmarksBarId
    <#> maybe Obj.empty Bookmarks.toUrlObj

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

first :: ∀ a. Array (Chrome a) -> Chrome a
first =
  map runExceptT
    .> parOneOf
    .> ExceptT
