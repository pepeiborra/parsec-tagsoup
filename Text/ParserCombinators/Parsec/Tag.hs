{-# LANGUAGE NoMonomorphismRestriction #-}

module Text.ParserCombinators.Parsec.Tag where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike
import Text.ParserCombinators.Parsec.Prim  hiding ((<|>), many)
import Text.ParserCombinators.Parsec.Combinator (many1)
import Text.ParserCombinators.Parsec.Pos

type TagParser str st = GenParser (Tag str) st

myTagToken :: (Show str, StringLike str) => (Tag str -> Maybe a) -> TagParser str st a
myTagToken = tokenPrim show updatePosTag
updatePosTag s _ _ = setSourceLine s ( 1 + sourceLine s )

satisfy :: (StringLike str, Show str) => (Tag str -> Bool) -> TagParser str st (Tag str)
satisfy f = myTagToken (\t -> if (f t) then Just t else Nothing)

tagOpenName, tagCloseName :: (Show str, StringLike str) => str -> TagParser str st (Tag str)
tagOpenName  = satisfy . isTagOpenName
tagCloseName = satisfy . isTagCloseName

tagOpen, tagClose, tagText :: (Show str, StringLike str) => TagParser str st (Tag str)
tagOpen  = satisfy isTagOpen
tagClose = satisfy isTagClose
tagText   = myTagToken (\tag -> case tag of
                                TagText _ -> Just tag
                                _         -> Nothing)
tag :: (StringLike str, Show str, TagRep str) => str -> TagParser str st (Tag str)
tag t = satisfy (~== t) <?> toString t

tag' :: (StringLike str, Show str, TagRep str) => (Tag str -> Bool) -> TagParser str st (Tag str)
tag' t = satisfy t <?> "TAG"

tagP :: (StringLike str, Show str, TagRep str) => str -> TagParser str st contents -> TagParser str st contents
tagP t p = (do
  tag t
  result <- p
  skipMany tagText
  let closing_tag = head $ words $ tail $ init $ toString t
  tag' (== TagClose (fromString closing_tag)) <?> closing_tag
  return result
  ) <?> toString t

anyTag :: (Show str, StringLike str) => TagParser str st (Tag str)
anyTag = myTagToken Just

elemTag :: StringLike str => Tag str -> [Tag str] -> Bool
elemTag tag = any (tag ~==)

oneOf, noneOf :: (Show str, StringLike str) => [Tag str] -> TagParser str st (Tag str)
oneOf ts  = satisfy (`elemTag` ts)
noneOf ts = satisfy (not . (`elemTag` ts))

skipTagP :: (Show str, StringLike str) => TagParser str st (Tag str)
skipTagP = do
  tag@(TagOpen name _) <- tagOpen
  skipMany ((tagText >> return ()) <|> (skipTagP >> return ()))
  tagCloseName name
  return tag

someTagP :: (Show str, StringLike str, TagRep str) => str -> TagParser str st a -> TagParser str st a
someTagP t k = (tagP t k <|> (skipTagP >> someTagP t k)) <* many skipTagP

skipTagNameP :: (StringLike str, Show str, TagRep str) => str -> TagParser str st ()
skipTagNameP name = do
  tag name
  skipMany ((tagText >> return ()) <|> (skipTagP >> return ()))
  let closing_tag = head $ words $ tail $ init $ toString name
  tag' (== TagClose (fromString closing_tag)) <?> closing_tag
  return ()

childrenP :: (Show str, StringLike str) => TagParser str st [Tag str]
childrenP = do
  open@(TagOpen name _) <- tagOpen
  content <- many (many1 tagText <|> childrenP)
  close <- tagCloseName name
  return (open : concat content ++ [close])
