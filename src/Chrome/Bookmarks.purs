module Chrome.Bookmarks where

import MasonPrelude
import Data.Argonaut
  ( class DecodeJson
  , JsonDecodeError(..)
  , Json
  , (.:)
  , (.:!)
  , (:=?)
  , (~>?)
  , decodeJson
  , encodeJson
  , jsonEmptyObject
  )
import Data.Argonaut as Arg
import Data.Array as Array
import Data.Newtype (class Newtype)
import Foreign.Object (Object)
import Foreign.Object as FO
import Chrome.Wrap (Chrome)
import Chrome.Wrap as Chrome

data BookmarkTreeNodeUnmodifiable
  = Managed

-- derive instance genericBookmarkTreeNodeUnmodifiable :: Generic BookmarkTreeNodeUnmodifiable _
instance decodeJsonBookmarkTreeNodeUnmodifiable :: DecodeJson BookmarkTreeNodeUnmodifiable where
  decodeJson json =
    Arg.toString json
      # maybe (Left $ TypeMismatch "I'm looking for a string here") case _ of
          "managed" -> Right Managed
          _ -> Left $ UnexpectedValue json

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

instance decodeJsonBookmarkTreeNode :: DecodeJson BookmarkTreeNode where
  decodeJson json =
    BookmarkTreeNode
      <$> do
          obj <- decodeJson json
          id <- obj .: "id"
          title <- obj .: "title"
          children <- obj .:! "children"
          dateAdded <- obj .:! "dateAdded"
          dateGroupModified <- obj .:! "dateGroupModified"
          index <- obj .:! "index"
          parentId <- obj .:! "parentId"
          unmodifiable <- obj .:! "unmodifiable"
          url <- obj .:! "url"
          pure
            { id
            , title
            , children
            , dateAdded
            , dateGroupModified
            , index
            , parentId
            , unmodifiable
            , url
            }

toUrlObj :: BookmarkTreeNode -> Object BookmarkTreeNode
toUrlObj = go FO.empty
  where
  go :: Object BookmarkTreeNode -> BookmarkTreeNode -> Object BookmarkTreeNode
  go acc btn@(BookmarkTreeNode r) = case r.children of
    Just children -> foldl go acc children
    Nothing -> case r.url of
      Just url -> FO.insert url btn acc
      Nothing -> acc

bookmarks :: ∀ a. DecodeJson a => String -> Array Json -> Chrome (Maybe a)
bookmarks = Chrome.wrapFailableApi "bookmarks" ~~$ Nothing

getTree :: Chrome (Array BookmarkTreeNode)
getTree = Chrome.wrapApi "bookmarks" "getTree" []

get :: Array String -> Chrome (Maybe (Array BookmarkTreeNode))
get ids = bookmarks "get" [ encodeJson ids ]

getOne :: String -> Chrome (Maybe BookmarkTreeNode)
getOne id = get [ id ] <#> bind ~$ Array.head

getChildren :: String -> Chrome (Maybe (Array BookmarkTreeNode))
getChildren id = bookmarks "getChildren" [ encodeJson id ]

getSubTree :: String -> Chrome (Maybe BookmarkTreeNode)
getSubTree id =
  bookmarks "getSubTree" [ encodeJson id ]
    <#> bind
    ~$ Array.head

move :: String -> { index :: Maybe Int, parentId :: Maybe String } -> Chrome (Maybe BookmarkTreeNode)
move id { index, parentId } =
  bookmarks "move"
    [ encodeJson id
    , "index" :=? index
        ~>? ("parentId" :=? parentId)
        ~>? jsonEmptyObject
    ]

onCreated :: Chrome { id :: String, bookmark :: BookmarkTreeNode }
onCreated =
  Chrome.wrapListener2 "bookmarks" "onCreated"
    { id: _, bookmark: _ }

type MoveInfo
  = { index :: Int
    , oldIndex :: Int
    , oldParentId :: String
    , parentId :: String
    }

onMoved :: Chrome { id :: String, moveInfo :: MoveInfo }
onMoved =
  Chrome.wrapListener2 "bookmarks" "onMoved"
    { id: _, moveInfo: _ }

newtype ChangeInfo
  = ChangeInfo { title :: String, url :: Maybe String }

instance decodeJsonChangeInfo :: DecodeJson ChangeInfo where
  decodeJson json =
    ChangeInfo
      <$> do
          obj <- decodeJson json
          title <- obj .: "title"
          url <- obj .:! "url"
          pure { title, url }

onChanged :: Chrome { id :: String, changeInfo :: ChangeInfo }
onChanged =
  Chrome.wrapListener2 "bookmarks" "onChanged"
    { id: _, changeInfo: _ }
