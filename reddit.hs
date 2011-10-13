{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.JSON
import Database.HDBC
import "mtl" Control.Monad.Trans (liftIO)
import Database.HDBC.Sqlite3
import Network.HTTP
import Control.Applicative
import Control.Arrow
import qualified Data.ByteString as BS
import Network.Curl.Download
import Data.List.Split (splitOn)
import Data.Either (either)
import Data.List (isSuffixOf)
import System.Directory (createDirectory,doesDirectoryExist)

{-Provides safety and downloads only these formats-}
imageFormats ::  [[Char]]
imageFormats = ["jpg","png","gif","jpeg"]

{-This is where you provide the reddit json urls-}
urls =  ["http://www.reddit.com/r/earthporn.json","http://www.reddit.com/r/pics.json"]

{-Generic json search to find a given key in the haskell JSValue object -}
genericSearch :: String -> JSValue -> [JSValue]
genericSearch key json= case json of 
                            (JSObject v) -> let result = lookup key (fromJSObject v)
                                            in case result of 
                                                    Nothing -> concatMap (genericSearch key.snd) (fromJSObject v)
                                                    Just s -> [s] ++ concatMap (genericSearch key.snd) (fromJSObject v)
                            (JSArray v) ->  concatMap (genericSearch key) v
                            _ -> []

{-helper function to get string out of JSString -}
fromJS (JSString v) = fromJSString v

{-function to save the file and add an instance in the database-}
write imgPath a conn= do 
            doc <- openURI $ snd a 
            case doc of 
                Left err -> putStrLn err
                Right img -> do 
                                putStrLn $ "writing " ++ fst a
                                BS.writeFile (imgPath ++ fst a) img
                                liftIO $ databaseInsert (snd a) conn

{-Call it only one time to create the required database-}
databaseCreation conn = do
                    run conn "CREATE TABLE images (number INTEGER PRIMARY KEY, url VARCHAR(200))" []
                    commit conn

{- Inserts url to the database.-}
databaseInsert url conn = do
                                        run conn "INSERT INTO images VALUES (?,?)" [SqlNull, toSql url]
                                        commit conn

{- Checks if a url already exists in the database.-}
checkDatabase url conn = do
                        a <- liftIO $ quickQuery conn "SELECT number FROM images WHERE url = ?" [toSql url]
                        return (a == [])


checkTable table conn = do
                        a <- liftIO $ quickQuery conn "SELECT name FROM sqlite_master WHERE type='table' AND name= ?" [toSql table]
                        return (a == [])
{-Checks if file url is not present in the database and saves it if not present-}
saveFile conn imgPath a@(name,url) = do 
                                 check <- liftIO $ checkDatabase url conn
                                 if check then write imgPath a conn  else putStrLn "Already in database"
                             
{-TODO -}
{-instead of checking in the database at the time of saving check at the time of downloading-}
download url = do
        let imgPath = "./"++ (head.splitOn ".".last $ splitOn "/" url) ++ "/"
        isImgPath <- doesDirectoryExist imgPath 
        if isImgPath then putStrLn (imgPath ++ " Exists So downloading into it. ")  else createDirectory imgPath
        conn <- liftIO $ connectSqlite3 "imageDatabase.db"
        isTable <- liftIO $ checkTable "images" conn
        if isTable then liftIO $ databaseCreation conn else putStrLn "Table exist so updating" 
        s<-simpleHTTP (getRequest url) >>= getResponseBody
        let (Ok value) = decode s :: Result JSValue
        let f x = (fmap fromJS.genericSearch x)
        let iurls = (f "url" value) 
        let names = fmap (last.splitOn "/") iurls
        let zipped = filter g $zip  names iurls
        sequence $ map (saveFile conn imgPath) zipped
        disconnect conn
    where 
        g (name,url) = any (flip isSuffixOf $ name) imageFormats  
main = sequence $ fmap download urls
