equiv_classes :: [a] -> (a -> a -> Bool) -> [[a]]
equiv_classes [] _ = []


classes [] _ = []
classes (x:xs) func = (equiv x xs func []) ++ (classes xs func)


equiv _ [] _ acc = acc
equiv a (x:xs) func acc = let acc' = if func a x then [a, x] : acc else acc
                          in equiv a xs func acc'



main :: IO()
main = do
    let tab = [1, 2, 3, 5, 6]
        myFunc = (\x y -> (x - y) `mod` 2 == 0)
    print(classes tab myFunc)