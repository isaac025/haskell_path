module MFM where

import Control.Concurrent.STM
import Control.Monad.ST
import Control.Monad (unless)
import Data.STRef
import Data.IORef

addName :: TVar Integer -> TVar [(Integer, String)] -> String -> STM ()
addName counter names name = do 
    i <- readTVar counter
    modifyTVar names ((i, name) :)
    writeTVar counter (i + 1)

--addName' :: TVar Integer -> TVar [(Integer, String)] -> String -> STM ()
addName' counter names name = do 
    i <- readIORef counter
    ls <- readIORef names
    modifyIORef names ((i, name):)
    writeIORef counter (i + 1)

-- Ex 8.1
addNameNonDuplicates counter names name = do 
    ls <- readIORef names 
    i <- readIORef counter
    case (name `elem` (map snd ls)) of 
        True -> return ()
        False -> do modifyIORef names ((i, name):)
                    writeIORef counter (i + 1)

weirdSum = do x <- newSTRef 5
              y <- newSTRef 2
              modifySTRef y (+1)
              (+) <$> readSTRef x <*> readSTRef y

weirdSum' x y = modifyIORef y (+2) >>= \_ -> (+) <$> readIORef x <*> readIORef y

main :: IO ()
main = do x <- newIORef [(2,"Carlos"), (1, "Juan")]
          c <- newIORef 3
          addNameNonDuplicates c x "Carlos"
          addNameNonDuplicates c x "Adrian"
          readIORef x >>= \x' -> 
            mapM_ (\x'' -> putStrLn ("Hello, " ++ (snd x'') ++ " you are number: " ++ (show (fst x'')))) (reverse x')

main2 :: IO ()
main2 = do let result1 = runST weirdSum
           x <- newIORef (5 :: Integer)
           y <- newIORef (2 :: Integer)
           x' <- readIORef x
           result2 <- weirdSum' x y
           putStrLn ("Result 1 is: " ++ show result1)
           putStrLn ("Result 2 is: " ++ show result2)

