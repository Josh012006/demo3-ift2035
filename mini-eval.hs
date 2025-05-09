data Exp = Enum Int -- Une constante
         | Eplus Exp Exp -- e1 + e2 
         | Etimes Exp Exp -- e1 * e2 
         | Eneg Exp -- (- e) 
         | Egt Exp Exp -- e1 > e2
         | Enot Exp -- (not e) 
         | Eif Exp Exp Exp -- if e1 then e2 else e3 


data Val = Vnum Int 
         | Vbool Bool 
        deriving(Show)


eval :: Exp -> Val
eval (Enum x) = changeInt (evalInt (Enum x))
eval (Eplus e1 e2) = changeInt (evalInt (Eplus e1 e2))
eval (Etimes e1 e2) = changeInt (evalInt (Etimes e1 e2))
eval (Eneg e) = changeInt (evalInt (Eneg e))
eval (Eif e1 e2 e3) = changeInt (evalInt (Eif e1 e2 e3))

eval (Egt e1 e2) = changeBool (evalBool (Egt e1 e2))
eval (Enot e) = changeBool (evalBool (Enot e))



changeInt :: Int -> Val
changeInt a = Vnum a

changeBool :: Bool -> Val
changeBool b = Vbool b


evalInt :: Exp -> Int
evalInt (Enum x) = x
evalInt (Eplus e1 e2) = (evalInt e1) + (evalInt e2)
evalInt (Etimes e1 e2) = (evalInt e1) * (evalInt e2)
evalInt (Eneg e) = 0 - (evalInt e)
evalInt (Eif e1 e2 e3) = if evalBool e1 then evalInt e2 else evalInt e3


evalBool :: Exp -> Bool
evalBool (Egt e1 e2) = (evalInt e1) > (evalInt e2)
evalBool (Enot e) = not (evalBool e)




main :: IO()
main = do
    print(eval(Eif (Enot (Egt (Etimes (Enum 5)(Enum 2)) (Enum 15))) (Eneg(Eplus(Enum 5) (Enum 8))) (Enum 9)))