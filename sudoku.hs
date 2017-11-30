module Sudoku where

{-
  This is inspired by John Hughes "Why Functional Programming Matters".
  We build a complete decision tree.
  That is, all alternatives in a certain depth
  have the same number of determined values.
  At the bottom of the tree all possible solutions can be found.
  Actually the algorithm is very stupid:
  In each depth we look for the field with the least admissible choices of numbers
  and prune the alternative branches for the other fields.
-}

import Data.Char (ord, chr, isDigit, digitToInt, intToDigit)
import Data.Array (Array, range, (!), (//))
import Data.Tree (Tree)
import qualified Data.Tree as Tree
import Data.List (sort, minimumBy)
import Data.Maybe (catMaybes, isNothing, fromMaybe, fromJust)
import qualified Data.Array as Array

{-
Example:

ghci -Wall Sudoku.hs

*Sudoku> mapM_ putCLn (solutions exampleHawiki0)
-}


{- [[ATree]] contains a list of possible alternatives for each position -}
data ATree a = ANode T [[ATree a]]

type Coord   = Int
type Address = (Int,Int,Int,Int)
type Element = Int

type T        = Array Address (Maybe Element)
type Complete = Array Address Element

fieldBounds :: (Address, Address)
fieldBounds = ((0,0,0,0), (2,2,2,2))

squareRange :: [(Coord, Coord)]
squareRange = range ((0,0), (2,2))

alphabet :: [Element]
alphabet = [1..9]



{- * solution -}

{-
  Given two sorted lists,
  remove the elements of the first list from the second one.
-}
deleteSorted :: Ord a => [a] -> [a] -> [a]
deleteSorted [] ys = ys
deleteSorted _  [] = []
deleteSorted (x:xs) (y:ys) =
   case compare x y of
      EQ -> deleteSorted xs ys
      LT -> deleteSorted xs (y:ys)
      GT -> y : deleteSorted (x:xs) ys

admissibleNumbers :: [[Maybe Element]] -> [Element]
admissibleNumbers =
   foldl (flip deleteSorted) alphabet .
   map (sort . catMaybes)

admissibleAdditions :: T -> Address -> [Element]
admissibleAdditions sudoku (i,j,k,l) =
   admissibleNumbers (map ($ sudoku)
      [selectRow    (i,k),
       selectColumn (j,l),
       selectSquare (i,j)])

allAdmissibleAdditions :: T -> [(Address, [Element])]
allAdmissibleAdditions sudoku =
   let adds addr =
          (addr, admissibleAdditions sudoku addr)
   in  map adds
           (map fst (filter (isNothing . snd)
                            (Array.assocs sudoku)))



solutionTree :: T -> ATree T
solutionTree sudoku =
   let new (addr,elms) =
          map (\elm -> solutionTree (sudoku // [(addr, Just elm)])) elms
   in  ANode sudoku (map new (allAdmissibleAdditions sudoku))

treeAltToStandard :: ATree T -> Tree T
treeAltToStandard (ANode sudoku subs) =
   Tree.Node sudoku (concatMap (map treeAltToStandard) subs)

{- Convert a tree with alternatives for each position (ATree)
   into a normal tree by choosing one position and its alternative values.
   We need to consider only one position per level
   because the remaining positions are processed in the sub-levels.
   With other words: Choosing more than one position
   would lead to multiple reports of the same solution.

   For reasons of efficiency
   we choose the position with the least number of alternatives.
   If this number is zero, the numbers tried so far are wrong.
   If this number is one, then the choice is unique, but maybe still wrong.
   If the number of alternatives is larger,
   we have to check each alternative.
-}
treeAltToStandardOptimize :: ATree T -> Tree T
treeAltToStandardOptimize (ANode sudoku subs) =
   let chooseMinLen [] = []
       chooseMinLen xs = minimumBy compareLength xs
   in  Tree.Node sudoku (chooseMinLen
          (map (map treeAltToStandardOptimize) subs))

maybeComplete :: T -> Maybe Complete
maybeComplete sudoku =
   fmap (Array.array fieldBounds)
        (mapM (uncurry (fmap . (,))) (Array.assocs sudoku))

{- All leafs are at the same depth,
   namely the number of undetermined fields.
   That's why we can safely select all Sudokus at the lowest level. -}
solutions :: T -> [Complete]
solutions sudoku =
   let err = error "The lowest level should contain complete Sudokus only."
       {- "last'" is more efficient than "last" here
          because the program does not have to check
          whether deeper levels exist.
          We know that the tree is as deep
          as the number of undefined fields.
          This means that dropMatch returns a singleton list.
          We don't check that
          because then we would lose the efficiency again. -}
       last' = head . dropMatch (filter isNothing (Array.elems sudoku))
   in  map (fromMaybe err . maybeComplete)
           (last' (Tree.levels
             (treeAltToStandardOptimize (solutionTree sudoku))))



{- * transformations (can be used for construction, too) -}

standard :: Complete
standard =
   Array.listArray fieldBounds
      (map (\(i,j,k,l) -> mod (j+k) 3 * 3 + mod (i+l) 3 + 1)
           (range fieldBounds))


exampleHawiki0, exampleHawiki1 :: T
exampleHawiki0 = fromString (unlines [
      " 5  6   1",
      "  48   7 ",
      "8      52",
      "2   57 3 ",
      "         ",
      " 3 69   5",
      "79      8",
      " 1   65  ",
      "5   3  6 "
   ])

exampleHawiki1 = fromString (unlines [
      " 7    9  ",
      "  4 5   1",
      "3  8   2 ",
      "  5  9 4 ",
      " 6  7   8",
      "2  1  3  ",
      "       5 ",
      " 1  4   7",
      "8  6     "
   ])




check :: Complete -> Bool
check sudoku =
   let checkParts select =
          all (\addr -> sort (select addr sudoku) == alphabet) squareRange
   in  all checkParts [selectRow, selectColumn, selectSquare]

selectRow, selectColumn, selectSquare ::
   (Coord,Coord) -> Array Address element -> [element]
selectRow (i,k) sudoku =
   map (sudoku!) (range ((i,0,k,0), (i,2,k,2)))
--   map (sudoku!) (map (\(j,l) -> (i,j,k,l)) squareRange)
selectColumn (j,l) sudoku =
   map (sudoku!) (range ((0,j,0,l), (2,j,2,l)))
selectSquare (i,j) sudoku =
   map (sudoku!) (range ((i,j,0,0), (i,j,2,2)))


{- * conversion from and to strings -}

put, putLn :: T -> IO ()
put   sudoku = putStr   (toString sudoku)
putLn sudoku = putStrLn (toString sudoku)

putC, putCLn :: Complete -> IO ()
putC   sudoku = putStr   (toString (fmap Just sudoku))
putCLn sudoku = putStrLn (toString (fmap Just sudoku))

fromString :: String -> T
fromString str =
   Array.array fieldBounds (concat
      (zipWith (\(i,k) -> map (\((j,l),x) -> ((i,j,k,l),x)))
         squareRange
         (map (zip squareRange . map charToElem) (lines str))))

toString :: T -> String
toString sudoku =
   unlines
      (map (\(i,k) -> map (\(j,l) -> elemToChar (sudoku!(i,j,k,l)))
                          squareRange)
           squareRange)

charToElem :: Char -> Maybe Element
charToElem c =
   toMaybe (isDigit c) (digitToInt c)

elemToChar :: Maybe Element -> Char
elemToChar =
   maybe ' ' intToDigit


{- * helper functions -}

nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = f (nest (n-1) f x)

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

compareLength :: [a] -> [b] -> Ordering
compareLength (_:xs) (_:ys) = compareLength xs ys
compareLength []     []     = EQ
compareLength (_:_)  []     = GT
compareLength []     (_:_)  = LT

{- | Drop as many elements as the first list is long -}
dropMatch :: [b] -> [a] -> [a]
dropMatch xs ys =
   map fromJust (dropWhile isNothing
      (zipWith (toMaybe . null) (iterate (drop 1) xs) ys))
