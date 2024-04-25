data Llong = Llong Integer

instance Show Llong where
    show (Llong x) = "long " ++ show x

data Lfloat = Lfloat Float

data Ldouble = Ldouble Double

data Lbool = Lbool Bool

data Lchar = Lchar Char

data Lstring = Lstring String

-- Jeito que funcionou os tipos recursivos:

data Type = Ltuple Type [Type] |  Lint Int

instance Show Type where
    show (Ltuple x (y:ys)) = show x ++ "," ++ show (Ltuple y ys)
    show (Ltuple x []) = show x
    show (Lint x) = show x
