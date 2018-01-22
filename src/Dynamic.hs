module Dynamic where

import qualified Data.Map.Strict as Map
import Control.Monad.State

-- ctci 8.1
child_steps :: Int -> Int
child_steps n = evalState (opt n) Map.empty
    where
        opt :: Int -> State (Map.Map Int Int) Int
        opt n 
            | n < 0 = return 0
            | n == 0 = return 1
            | otherwise = do
                value <- Map.lookup n <$> get
                case value of
                    Just rv -> return rv
                    Nothing -> do
                        a <- opt (n-3)
                        b <- opt (n-2)
                        c <- opt (n-1)
                        let rv = a + b + c
                        Map.insert n rv <$> get >>= put
                        return rv

