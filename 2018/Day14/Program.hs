import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.MArray
import Data.Array.ST
import Data.Word

main :: IO ()
main = do
    print solve

input :: Int
input = 509671

recipeCount :: Int
recipeCount = input + 10

solve :: Int
solve = read $ concatMap show $ take 10 $ drop input $ elems recipes

recipes :: UArray Int Word8
recipes = runSTUArray $ do
    result <- newListArray (0, recipeCount) [3, 7]
    loop 0 1 result 2
    return result

loop :: Int -> Int -> STUArray s Int Word8 -> Int -> ST s ()
loop recipeA recipeB currentRecipes recipesLength
    | recipesLength >= recipeCount = return ()
    | otherwise = do
        recipeAScore <- readArray currentRecipes recipeA
        recipeBScore <- readArray currentRecipes recipeB
        recipesLength' <- appendDigitSum currentRecipes recipesLength recipeAScore recipeBScore
        let recipeA' = (recipeA + fromIntegral recipeAScore + 1) `rem` recipesLength'
        let recipeB' = (recipeB + fromIntegral recipeBScore + 1) `rem` recipesLength'
        loop recipeA' recipeB' currentRecipes recipesLength'

    where
        appendDigitSum a i x y
            | x < 0 || y < 0 = return $ error $ "Expected x and y both to be positive numbers (x = " ++ show x ++ ", y = " ++ show y ++ ")"
            | x + y >= 20 = return $ error $ "Expected x and y both to be single digits (x = " ++ show x ++ ", y = " ++ show y ++ ")"
            | x + y >= 10 = do
                writeArray a i 1
                writeArray a (i + 1) (x + y - 10)
                return $ i + 2
            | otherwise = do
                writeArray a i (x + y)
                return $ i + 1
