x = [(a,b) | a <- [1,2,3], b <- [4,5,6], a + 2 < b]

x' = do
 a <- [1,2,3]
 b <- [4,5,6]
 True <- return $ a + 2 < b
 return (a,b)

x'' = [1,2,3] >>= (\a -> [4,5,6] >>= (\b -> return (a + 2 < b) >>= 
 (\r -> case r of True -> return (a,b)
                  _    -> fail $ show (a,b))))

y = [(a,b) | a <- [1,2,3], b <- [4,5,6], (a,b) == (1,5)]

y' = do
 a <- [1,2,3]
 b <- [4,5,6]
 (1,5) <- return $ (a,b)
 return (a,b)

y'' = [1,2,3] >>= (\a -> [4,5,6] >>= (\b -> return ( (a,b) == (1,5) ) >>=
        (\r -> case r of True -> return (a,b)
                         _    -> fail $ show (a,b))))

z' = do
 a <- [1,2,3]
 b <- [4,5,6]
 (1,_) <- return $ (a,b)
 return (a,b)



w'' = [1] >>= (\a -> fail $ show "If you see this message, destroy the computer!")

q'' = [1,2] >>= (\a -> case a of 1 -> return "hooray"
                                 2 -> fail "Probably don't destroy the compy.")   
