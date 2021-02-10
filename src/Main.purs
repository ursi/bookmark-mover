module Main where

import MasonPrelude
import Chrome.Bookmarks (BookmarkTreeNode(..))
import Chrome.Bookmarks as Bookmarks
import Chrome.WebNavigation (TransitionType(..))
import Chrome.WebNavigation as WebNav
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Data.List ((:))
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Newtype (unwrap)
import Debug as Debug
import Effect.Aff (launchAff_)
import Foreign.Object (Object)
import Foreign.Object as Obj
import FRP.Event as Event

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
                                [ Navigated <$> something
                                -- ^^ don't use parLift2 because the onUpdate listener gets called multiple times ^^ also the comment is down here because purty sux
                                , BookmarkCreated <$> Bookmarks.onCreated
                                , BookmarkMoved <$> Bookmarks.onMoved
                                , BookmarkChanged <$> Bookmarks.onChanged
                                ]
                        case event of
                          Navigated { url, transitionType } -> do
                            when (transitionType /= Reload)
                              $ ( lift
                                    $ runMaybeT do
                                        bookmark <- MaybeT $ pure $ Obj.lookup url bookmarks
                                        lift
                                          $ getLineage bookmark
                                          >>= traverse_
                                              ( \id ->
                                                  runMaybeT do
                                                    index <- (MaybeT $ Bookmarks.getOne id) <#> unwrap <#> _.index
                                                    lift
                                                      $ Bookmarks.move id
                                                          { index: index <#> (_ / 2)
                                                          , parentId: Nothing
                                                          }
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
  = Navigated { url :: String, transitionType :: TransitionType }
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

something :: Chrome { url :: String, transitionType :: TransitionType }
something =
  Event.sampleOn
    (runExceptT WebNav.onBeforeNavigateE)
    (runExceptT WebNav.onCommittedE <#> \ocDets obnDets -> lift2 Tuple obnDets ocDets)
    # ExceptT
    # Chrome.toChrome
    <#> \({ url } /\ { transitionType }) -> { url, transitionType }
