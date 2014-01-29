{-# OPTIONS_GHC -XFlexibleContexts #-}

import Control.Monad.State

xs = [1, 2, 1, 4]

letters n = take n $ repeat 'A'

accumLetters n ys = (letters n):ys

ys = foldr accumLetters [] xs

-- main = print ys

capitalLetters = take 26 ['A'..]

accumLetters' n = do
                (letters, xs) <- get
                put $ (tail letters, (take n $ repeat $ head letters):xs)

states :: MonadState ([Char], [[Char]]) m => [m ()]
states = map accumLetters' xs

ys' = reverse $ snd $ execState (sequence states) (capitalLetters, [])

main = print ys'



accumLetters'' (n, thisLetter) ys =
    (take n $ repeat $ thisLetter):ys

ys'' = foldr accumLetters'' [] $ zip xs capitalLetters
