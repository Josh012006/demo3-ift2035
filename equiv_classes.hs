equiv_classes :: Eq a => [a] -> (a -> a -> Bool) -> [[a]]
equiv_classes [] _ = []
equiv_classes xs func = rmv_dupl (classes xs func) []


eq :: Eq a => [a] -> [a] -> Bool
eq [] [] = True
eq (x:xs) ys = (length ys == length (x:xs)) && (elem x ys) && (eq xs (filter (\a -> a /= x) ys))


rmv_dupl :: Eq a => [[a]] -> [[a]] -> [[a]]
rmv_dupl [] acc = acc
rmv_dupl (x:xs) acc = let rmv' a tab = filter (\y -> not (eq a y)) tab
                          acc' = x : acc
                        in rmv_dupl (rmv' x xs) acc'  


classes :: [a] -> (a -> a -> Bool) -> [[a]]
classes [] _ = []
classes xs func = [(equiv x xs func []) | x <- xs]


equiv :: a -> [a] -> (a -> a -> Bool) -> [a] -> [a]
equiv _ [] _ acc = acc
equiv a (x:xs) func acc = let acc' = if func a x then x : acc else acc
                          in equiv a xs func acc'



main :: IO()
main = do
    let tab = [1, 2, 3, 5, 6]
        myFunc = (\x y -> (x - y) `mod` 2 == 0)
    print(equiv_classes tab myFunc)