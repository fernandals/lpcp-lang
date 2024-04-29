data Type = Tuple Type [Type] |  Int Int | Long Integer | Float Float | Double Double | Bool Bool | Char Char | String String

instance Show Type where
    show (Tuple x (y:ys)) = show x  ++ "," ++ show (Tuple y ys)
    show (Tuple x []) = show x
    show (Int x) = show x
    show (Long x) = show x
    show (Float x) = show x
    show (Double x) = show x
    show (Bool x) = show x
    show (Char x) = show x
    show (String x) = show x

   
 