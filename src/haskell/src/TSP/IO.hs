-- | TSP.IO: parsing of *.tsp instance files.
-- Format: # comments, first significant line N, then N lines of N integers each.
module TSP.IO
  ( Matrix
  , readInstanceFile
  , validateMatrix
  ) where

import Data.Char (isSpace)

-- | Matrix: list of rows, 1-indexed conceptually. Row i = matrix !! (i-1).
type Matrix = [[Int]]

-- | Read *.tsp file. Ignores lines starting with # and empty lines.
-- First significant line = N, next N lines = matrix rows.
readInstanceFile :: FilePath -> IO (Either String (Int, Matrix))
readInstanceFile path = do
  contents <- readFile path
  let lines' = lines contents
  let significant = filter (not . isCommentOrEmpty) lines'
  case significant of
    [] -> return $ Left "No significant lines"
    (nStr : rest) ->
      case reads nStr of
        [(n, _)] | n > 0 ->
          if length rest >= n
            then do
              let rows = take n rest
              case parseMatrixRows n rows of
                Right m -> return $ Right (n, m)
                Left err -> return $ Left err
            else return $ Left "Not enough matrix rows"
        _ -> return $ Left "Invalid N"

isCommentOrEmpty :: String -> Bool
isCommentOrEmpty l
  | all isSpace l = True
  | otherwise = case dropWhile isSpace l of
      ('#' : _) -> True
      _ -> False

parseMatrixRows :: Int -> [String] -> Either String Matrix
parseMatrixRows n rows =
  let parsed = map (map read . words) rows
   in if all ((== n) . length) parsed
        then Right parsed
        else Left "Invalid row lengths"

-- | Validate: N rows, N cols, non-negative, diagonal 0.
validateMatrix :: Int -> Matrix -> Either String ()
validateMatrix n m
  | length m /= n = Left "Wrong number of rows"
  | any ((/= n) . length) m = Left "Wrong row length"
  | any (< 0) (concat m) = Left "Negative distance"
  | any (\(i, row) -> row !! (i - 1) /= 0) (zip [1 .. n] m) =
      Left "Diagonal must be 0"
  | otherwise = Right ()
