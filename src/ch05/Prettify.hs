module Prettify where

import Prelude hiding ((<>))

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y = x `Concat` y

concat :: [[a]] -> [a]
concat = foldr (++) []

hcat :: [Doc] -> Doc
hcat = foldr (<>) empty

fsep :: [Doc] -> Doc
fsep = foldr (</>) empty

(</>) :: Doc -> Doc -> Doc
Empty </> y = y
x </> Empty = x
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line = Char ' '
flatten (x `Union` _) = flatten x
flatten other = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d : ds) = (d <> p) : punctuate p ds

compact :: Doc -> String
compact x = transform [x]
  where
    transform [] = ""
    transform (d : ds) =
      case d of
        Empty -> transform ds
        Char c -> c : transform ds
        Text s -> s ++ transform ds
        Line -> '\n' : transform ds
        a `Concat` b -> transform (a : b : ds)
        _ `Union` b -> transform (b : ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where
    best col (d : ds) =
      case d of
        Empty -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line -> '\n' : best 0 ds
        a `Concat` b -> best col (a : b : ds)
        a `Union` b ->
          nicest
            col
            (best col (a : ds))
            (best col (b : ds))
    best _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` "" = True
w `fits` ('\n' : _) = True
w `fits` (c : cs) = (w - 1) `fits` cs

nest :: Int -> Doc -> String
nest width x = best 0 0 [] [x]
  where
    best col level open (d : ds) =
      case d of
        Empty -> best_ col ds
        Char '{' -> open_bracket '{'
        Char '}' -> close_bracket '}'
        Char '[' -> open_bracket '['
        Char ']' -> close_bracket ']'
        Char c -> c : best_ (col + 1) ds
        Text s -> s ++ best_ (col + length s) ds
        Line -> '\n' : replicate (4 * level) ' ' ++ best_ (4 * level) ds
        a `Concat` b -> best_ col (a : b : ds)
        a `Union` b -> nicest col (best_ col (a : ds)) (best_ col (b : ds))
      where
        best_ col = best col level open
        open_bracket b =
          nicest
            col
            (b : best (col + 1) level (False : open) ds)
            (b : "\n" ++ replicate (4 * (level + 1)) ' ' ++ best (4 * (level + 1)) (level + 1) (True : open) ds)
        close_bracket b =
          if head open
            then '\n' : replicate (4 * (level - 1)) ' ' ++ b : best (4 * (level - 1) + 1) (level - 1) (tail open) ds
            else b : best (col + 1) level (tail open) ds
    best _ _ _ _ = ""

    nicest col a b
      | (width - least) `fits` a = a
      | otherwise = b
      where
        least = min width col
