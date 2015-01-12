-- Name: Ben Kogan

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.

type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.

type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.

type STemplate = Template

blank :: Char
blank = '?'

-- Check that a template can be matched to a word using a given hand (ex. 3)

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ ws = False
wordFitsTemplate ts _ [] = False
wordFitsTemplate (t:ts) hs (w:ws)
  | t == w                      = wordFitsTemplate ts hs ws
  | t == blank && (w `elem` hs) = wordFitsTemplate ts (delete w hs) ws
  | otherwise                   = False

-- Find all words formable from a hand (ex. 2)

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Determine if a word is formable by a hand (ex. 1)

formableBy :: String -> Hand -> Bool
formableBy [] hand = True
formableBy (w:ws) hand
  | w `elem` hand = formableBy ws (delete w hand)
  | otherwise     = False

