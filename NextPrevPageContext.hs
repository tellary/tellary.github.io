module NextPrevPageContext(nextPrevPageContext) where

import Data.Functor ((<&>))
import Data.Monoid  ((<>))
import Hakyll

url :: Identifier -> Compiler String
url id = do
  let empty' = fail $ "No route url found for item " ++ show id
  fmap (maybe empty' toUrl) $ getRoute id

title :: Identifier -> Compiler String
title id = do
  titleMaybe <- getMetadataField id "title"
  case titleMaybe of
    Just title -> return title
    Nothing    -> fail $ "No title in " ++ show id

nextPrevPageContext :: Pattern -> Context a
nextPrevPageContext ptrn
  =  field "nextUrl"   (\i -> (findNext . itemIdentifier $ i) >>= url  )
  <> field "prevUrl"   (\i -> (findPrev . itemIdentifier $ i) >>= url  )
  <> field "nextTitle" (\i -> (findNext . itemIdentifier $ i) >>= title)
  <> field "prevTitle" (\i -> (findPrev . itemIdentifier $ i) >>= title)
  where
    nextPrevById
      :: Compiler [(Identifier, (Maybe Identifier, Maybe Identifier))]
    nextPrevById = do
      ids <- sortRecentFirst =<< getMatches ptrn
      let prevs = (map Just $ tail ids) ++ [Nothing]
      let nexts = [Nothing] ++ (map Just $ init ids)
      return $ zip ids (zip nexts prevs)
    findNextPrev itemId
      =   nextPrevById
      <&> lookup itemId
      <&> maybe
          (error $ "Can't find page by " ++ show itemId)
          id
    findNext :: Identifier -> Compiler Identifier
    findNext itemId
      =   findNextPrev itemId
      <&> fst
      >>= maybe
          (noResult $ "No next page for " ++ show itemId)
          return
    findPrev :: Identifier -> Compiler Identifier
    findPrev itemId
      =   findNextPrev itemId
      <&> snd
      >>= maybe
          (noResult $ "No previous page for " ++ show itemId)
          return
