{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.ParserCombinators.Parsec.Tag
 ( TagParser
 , satisfy, lexeme
 , space, whitespace
 , anyTag, anyTagOpen, anyTagClose
 , tag, tagOpen, tagClose
 , tagP, anyTagP
 , tagText
 , oneOf, noneOf
 ) where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Maybe
import Text.HTML.TagSoup
import Text.StringLike
import Text.ParserCombinators.Parsec.Prim  hiding ((<|>), many)
import Text.ParserCombinators.Parsec.Combinator (many1)
import Text.ParserCombinators.Parsec.Pos

-- | The type of Tag parsers, a synonym for GenParser (Tag str)
type TagParser str st = GenParser (Tag str) st

-- | The primitive tag token parser
tagToken :: (Show str, StringLike str) => (Tag str -> Maybe a) -> TagParser str st a
tagToken = tokenPrim show updatePosTag
  where updatePosTag s _ _ = setSourceLine s ( 1 + sourceLine s )

-- | Parse a tag if it satisfies the predicate.
--   As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
satisfy :: (StringLike str, Show str) => (Tag str -> Bool) -> TagParser str st (Tag str)
satisfy f = lexeme $ tagToken (\t -> if (f t) then Just t else Nothing)


-- | Parse the given opening or closing tag
--   As all the tag parsers, these consume the whitespace immediately after the parsed tag.
tagOpen, tagClose :: (Show str, StringLike str) => str -> TagParser str st (Tag str)
tagOpen  x = satisfy (isTagOpenName  x) <?> show (TagOpen x [])
tagClose x = satisfy (isTagCloseName x) <?> show (TagClose x)

-- | Parse any opening or closing tag.
--   As all the tag parsers, these consume the whitespace immediately after the parsed tag.
anyTagOpen, anyTagClose :: (Show str, StringLike str) => TagParser str st (Tag str)
anyTagOpen  = satisfy isTagOpen  <?> "<TAG>"
anyTagClose = satisfy isTagClose <?> "</TAG>"

-- | Parses a chunk of text.
--   As all the tag parsers, it consumes the whitespace immediately after the parsed tag.
tagText :: (Show str, StringLike str) => TagParser str st str
tagText   = (lexeme $
             tagToken (\tag -> case tag of
                                TagText x -> Just x
                                _         -> Nothing))
            <?> "TEXT"

-- | Parses a text block containing only characters which satisfy 'isSpace'.
space :: (Show str, StringLike str) => TagParser str st (Tag str)
space = satisfy (\tag -> case tag of
                                TagText x | all isSpace (toString x) -> True
                                _                                    -> False )

-- | Parses any whitespace. Whitespace consists of zero or more ocurrences of 'space'.
whitespace :: (Show str, StringLike str) => TagParser str st ()
whitespace = skipMany space

-- | @lexeme p@ first applies parser @p@ and then 'whitespace', returning the value of @p@.
--    
--    @lexeme = (<* whitespace)@
--    
--   Every tag parser is defined using 'lexeme', this way every parse starts at a point
--   without whitespace.
-- 
--   The only point where 'whitespace' should be called explicitly is at the start of
--   the top level parser, in order to skip any leading whitespace.

lexeme :: (Show str, StringLike str) => TagParser str st a -> TagParser str st a
lexeme p = p <* whitespace

-- | @tag t@ parses any tag for which @(~== t)@ is true.
tag :: forall st str rep . (StringLike str, Show str, TagRep rep) => rep -> TagParser str st (Tag str)
tag t = satisfy (~== t) <?> show(toTagRep t :: Tag str)

-- | Parses any tag for which the predicate is true.
tag' :: (StringLike str, Show str, TagRep str) => (Tag str -> Bool) -> TagParser str st (Tag str)
tag' t = satisfy t <?> "TAG"

-- | The main workhorse.
--
--   @tagP t p@ parses an opening tag @u@ for which @(~== t)@ is true.
--   Then it runs the continuation parser @p@.
--   Next it skips all the tags until the closing tag for @u@.
--   Finally it returns the results of @p@.
-- 
--   The @p@ parser should never consume the closing tag for @u@, or tagP will fail.

tagP :: forall rep str st a. (StringLike str, Show str, TagRep rep) => rep -> (Tag str -> TagParser str st a) -> TagParser str st a
tagP t p = lexeme (do
  x@(TagOpen name _) <- tag t
  result <- p x
  skipMany (satisfy (/= TagClose name))--  skipMany tagText
  tagClose name
  return result
  ) <?> show (toTagRep t :: Tag str)

-- | Parses any tag.
anyTag :: (Show str, StringLike str) => TagParser str st (Tag str)
anyTag = lexeme $ tagToken Just

-- | Behaves like @tagP@ sans the predicate. Expects that the next tag will be an opening tag or fails otherwise.
anyTagP :: (Show str, StringLike str) => (Tag str -> TagParser str st a) -> TagParser str st a
anyTagP p = lexeme $ do
  x@(TagOpen name _) <- anyTagOpen
  result <- p x
  skipMany (satisfy (/= TagClose name))
  tagClose name
  return result

elemTag :: StringLike str => Tag str -> [Tag str] -> Bool
elemTag tag = any (tag ~==)

-- | Versions of these Parsec combinators for tags
oneOf, noneOf :: (Show str, StringLike str) => [Tag str] -> TagParser str st (Tag str)
oneOf ts  = satisfy (`elemTag` ts)
noneOf ts = satisfy (not . (`elemTag` ts))

-- | @someTagP t k@ skips tags until it finds an opening tag that satisfies @(~== t)@. From there on it  behaves like 'tagP'.
someTagP :: (Show str, StringLike str, TagRep str) => str -> (Tag str -> TagParser str st a) -> TagParser str st a
someTagP t k = skipMany (satisfy (~/= t)) >>  tagP t k
--someTagP t k = (tagP t k <|> (skipTagP >> someTagP t k)) <* many skipTagP

childrenP :: (Show str, StringLike str) => TagParser str st [Tag str]
childrenP = do
  open@(TagOpen name _) <- anyTagOpen
  content <- many (many1 (TagText <$> tagText) <|> childrenP)
  close <- tagClose name
  return (open : concat content ++ [close])
