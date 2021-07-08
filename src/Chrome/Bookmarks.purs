module Chrome.Bookmarks where

import MasonPrelude
import Control.Monad.Except (throwError)
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome
import Data.Array as Array
import Data.Newtype (class Newtype)
import Foreign (Foreign, ForeignError(..))
import Foreign.Object (Object)
import Foreign.Object as FO
import Simple.JSON as Json
import Simple.JSON (class ReadForeign, readImpl)

data BookmarkTreeNodeUnmodifiable
  = Managed

-- derive instance genericBookmarkTreeNodeUnmodifiable :: Generic BookmarkTreeNodeUnmodifiable _
instance readForeignBookmarkTreeNodeUnmodifiable :: ReadForeign BookmarkTreeNodeUnmodifiable where
  readImpl =
    Json.read'
    >=> case _ of
          "managed" -> pure Managed
          str -> throwError $ pure $ TypeMismatch "managed" str

newtype BookmarkTreeNode
  = BookmarkTreeNode
  { id :: String
  , title :: String
  , children :: Maybe (Array BookmarkTreeNode)
  , dateAdded :: Maybe Number
  , dateGroupModified :: Maybe Number
  , index :: Maybe Int
  , parentId :: Maybe String
  , unmodifiable :: Maybe String
  , url :: Maybe String
  }

derive instance newtypeBookmarkTreeNode :: Newtype BookmarkTreeNode _

instance showBookmarkTreeNode :: Show BookmarkTreeNode where
  show (BookmarkTreeNode r) = show r

instance readForeignBookmarkTreeNode :: ReadForeign BookmarkTreeNode where
  readImpl foreign_ = BookmarkTreeNode <$> readImpl foreign_

toUrlObj :: BookmarkTreeNode -> Object BookmarkTreeNode
toUrlObj = go FO.empty
  where
  go :: Object BookmarkTreeNode -> BookmarkTreeNode -> Object BookmarkTreeNode
  go acc btn@(BookmarkTreeNode r) =
    case r.children of
      Just children -> foldl go acc children

      Nothing ->
        case r.url of
          Just url -> FO.insert url btn acc
          Nothing -> acc

bookmarks :: ∀ a. ReadForeign a => String -> Array Foreign -> Chrome (Maybe a)
bookmarks = Chrome.wrapFailableApi "bookmarks" ~~$ Nothing

getTree :: Chrome (Array BookmarkTreeNode)
getTree = Chrome.wrapApi "bookmarks" "getTree" []

get :: Array String -> Chrome (Maybe (Array BookmarkTreeNode))
get ids = bookmarks "get" [ Json.write ids ]

getOne :: String -> Chrome (Maybe BookmarkTreeNode)
getOne id = get [ id ] <#> bind ~$ Array.head

getChildren :: String -> Chrome (Maybe (Array BookmarkTreeNode))
getChildren id = bookmarks "getChildren" [ Json.write id ]

getSubTree :: String -> Chrome (Maybe BookmarkTreeNode)
getSubTree id =
  bookmarks "getSubTree" [ Json.write id ]
  <#> bind ~$ Array.head

move :: String -> { index :: Maybe Int, parentId :: Maybe String } -> Chrome (Maybe BookmarkTreeNode)
move id moveDetails =
  bookmarks "move"
    [ Json.write id
    , Json.write moveDetails
    ]

onCreated :: Chrome { id :: String, bookmark :: BookmarkTreeNode }
onCreated =
  Chrome.wrapListener2
    { id: _, bookmark: _ }
    "bookmarks"
    "onCreated"

type MoveInfo
  = { index :: Int
    , oldIndex :: Int
    , oldParentId :: String
    , parentId :: String
    }

onMoved :: Chrome { id :: String, moveInfo :: MoveInfo }
onMoved =
  Chrome.wrapListener2
    { id: _, moveInfo: _ }
    "bookmarks"
    "onMoved"

type ChangeInfo
  = { title :: String, url :: Maybe String }

onChanged :: Chrome { id :: String, changeInfo :: ChangeInfo }
onChanged =
  Chrome.wrapListener2
    { id: _, changeInfo: _ }
    "bookmarks"
    "onChanged"
