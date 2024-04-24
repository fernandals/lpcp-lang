module Data.Array.IArray

data int = int Int

data long = long Integer

data float = float Float

data double = double Double

data bool = bool Bool

data char = char Char

data string = string String

data Type = tuple | list | int | ...

-- Recursivos:
data tuple = tuple int * [Type]

data list = list String * [Type]

data matrix = 

data maybe = maybe String * nothing | maybe String * just Type

data either = either String * String * left Type | either String * String * right Type

data table = table 

dt : table<char, int> = {'a'}

et : either<int, float> = left 2

mb : maybe<int> = Just 1

numeros : list<int> = [1, 2, 4]

data list = list String * [Type]







