{-# LANGUAGE ScopedTypeVariables #-}

module YearMonthArchiveContext(yearMonthArchiveContext) where

import Control.Monad   (MonadFail, mapM)
import Data.Binary     (Binary)
import Data.List       (groupBy)
import Data.Typeable   (Typeable)
import Hakyll
import System.FilePath (takeBaseName)
import System.Time     (Month)
import Text.Regex      (matchRegex, mkRegex)

type Year = Int

yearMonthArchiveContext
  :: Pattern -> Context String
yearMonthArchiveContext ptrn
  =  listField "years"
     (    field "year" (return . fst . itemBody)
       <> listFieldWith "months"
          (    field "month" (return . fst . itemBody)
            <> listFieldWith "monthPosts"
               (    defaultContext
                 <> dateField "dayOfMonth" "%e"
               )
               (return . snd . itemBody)
          )
          (toItems . snd . itemBody)
     )
     (toItems =<< byYearMonth)
  <> bodyField "body"
  where
    byYearMonth :: Compiler [(Year, [(Month, [Item String])])]
    byYearMonth = do
      posts <- recentFirst =<< (loadAll ptrn :: Compiler [Item String])
      groupYearMonthPost <$> mapM yearMonthPost posts
    toItems :: Show b => [(b, c)] -> Compiler [Item (String, c)]
    toItems bcs = sequence <$> makeItem ((\(b, c) -> (show b, c)) <$> bcs)

equating :: Eq b => (a -> b) -> a -> a -> Bool
equating f a b = f a == f b

yearMonthPost :: MonadFail m => Item a -> m (Year, (Month, Item a))
yearMonthPost post = do
  year  <- postYear file
  month <- postMonth file
  return (year, (month, post))
  where
    path = toFilePath . itemIdentifier $ post
    file = takeBaseName path

postRegex = mkRegex "^([0-9]{4})\\-([0-9]{2})\\-([0-9]{2})\\-(.+)$"

postYear :: MonadFail m => FilePath -> m Year
postYear path
  = case read . head <$> matchRegex postRegex path of
      Just year -> return year
      Nothing -> fail $ "Can't parse year: " ++ path


postMonth :: MonadFail m => FilePath -> m Month
postMonth path
    = case read . head . tail <$> matchRegex postRegex path of
      Just month -> return . toEnum $ month - 1
      Nothing -> fail $ "Can't parse month: " ++ path

groupByFst :: Eq a => [(a, b)] -> [(a, [b])]
groupByFst
  = map (\group -> (fst . head $ group, map snd group)) . groupBy (equating fst)

groupYearMonthPost
  :: forall a . [(Year, (Month, Item a))] -> [(Year, [(Month, [Item a])])]
groupYearMonthPost ymPosts
  = fmap groupByFst <$> byYear
  where
    byYear :: [(Year, [(Month, Item a)])]
    byYear = groupByFst ymPosts
