{-# LANGUAGE
    FlexibleInstances,
    MultiParamTypeClasses
#-}
module Math.Instance.Module where
import Math.Module

-- newtype Vect k b = V [(b,k)] deriving (Eq,Ord)

-- instance (Show k, Eq k, Num k, Show b) => Show (Vect k b) where
--     show (V []) = "0"
--     show (V ts) = concatWithPlus $ map showTerm ts
--         where showTerm (b,x) | show b == "1" = show x
--                              | show x == "1" = show b
--                              | show x == "-1" = "-" ++ show b
--                              | otherwise = (if isAtomic (show x) then show x else "(" ++ show x ++ ")") ++
--                                            show b
--                                            -- (if ' ' `notElem` show b then show b else "(" ++ show b ++ ")")
--                                            -- if we put this here we miss the two cases above
--               concatWithPlus (t1:t2:ts) = if head t2 == '-'
--                                           then t1 ++ concatWithPlus (t2:ts)
--                                           else t1 ++ '+' : concatWithPlus (t2:ts)
--               concatWithPlus [t] = t
--               isAtomic (c:cs) = isAtomic' cs
--               isAtomic' ('^':'-':cs) = isAtomic' cs
--               isAtomic' ('+':cs) = False
--               isAtomic' ('-':cs) = False
--               isAtomic' (c:cs) = isAtomic' cs
--               isAtomic' [] = True


-- instance Module k (Vect k b) where
--   (*>) = smultL
