{-# OPTIONS_GHC -XFlexibleContexts #-}

import Control.Monad.State

xs = [1, 2, 1, 4]


letters n = take n $ repeat 'A'

accumLetters n ys = (letters n):ys

ys = foldr accumLetters [] xs


capitalLetters = take 26 ['A'..]

accumLetters' n = do
                (letters, xs) <- get
                put $ (tail letters, (take n $ repeat $ head letters):xs)

ys' :: MonadState ([Char], [[Char]]) m => [m ()]
ys' = map accumLetters' xs

main = print $ reverse $ snd $ execState (sequence  ys') (capitalLetters, [])