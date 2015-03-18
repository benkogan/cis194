-- Name: Ben Kogan

module HW02 where

import Words
import Data.List
import Data.Ord

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

-- Compute value of a word on a given template, inc. all multipliers (ex. 7)

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate ts ws
  | tripleWd `elem` ts = 3 * wordValue ts ws
  | doubleWd `elem` ts = 2 * wordValue ts ws
  | otherwise          = 1 * wordValue ts ws
  where
    tripleWd = 'T'
    doubleWd = 'D'

-- Compute value of a word on a given template, inc. only square multipliers

wordValue :: STemplate -> String -> Int
wordValue [] [] = 0
wordValue (t:ts) (w:ws)
  | t == tripleSq = 3 * scrabbleValue w + wordValue ts ws
  | t == doubleSq = 2 * scrabbleValue w + wordValue ts ws
  | otherwise     = 1 * scrabbleValue w + wordValue ts ws
  where
    tripleSq = '3'
    doubleSq = '2'

-- Find maximum scoring words from a list of words (ex. 6)

bestWords :: [String] -> [String]
bestWords xs = filter equalsMax xs
  where equalsMax x  = scrabbleValueWord x == maxScore
        maxScore     = scrabbleValueWord maxScoreWord
        maxScoreWord = maximumBy (comparing scrabbleValueWord) xs

-- Find the point value of a word (ex. 5)

scrabbleValueWord :: String -> Int
scrabbleValueWord ws = sum $ map scrabbleValue ws

-- Find all words formable from a hand and a given template (ex. 4)

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate ts hs = filter (wordFitsTemplate ts hs) allWords

-- Check that a template can be matched to a word using a given hand (ex. 3)

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _  = False
wordFitsTemplate  _ _ [] = False
wordFitsTemplate ts hs ws = wordFits ts hs ws

-- Same as above, but accepts words of lesser length than template too (ex. 8)

wordFitsTemplate' :: Template -> Hand -> String -> Bool
wordFitsTemplate' _ _ [] = True
wordFitsTemplate' [] _ _ = False
wordFitsTemplate' ts hs ws = wordFits ts hs ws

-- Template-matching logic for `wordFitsTemplate` and `wordFitsTemplate'`

wordFits :: Template -> Hand -> String -> Bool
wordFits (t:ts) hs (w:ws)
  | t == w                      = wordFits ts hs ws
  | t == blank && (w `elem` hs) = wordFits ts (delete w hs) ws
  | otherwise                   = False
  where blank = '?'

-- Find all words formable from a hand (ex. 2)

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Determine if a word is formable by a hand (ex. 1)

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (w:ws) hand
  | w `elem` hand = formableBy ws (delete w hand)
  | otherwise     = False

