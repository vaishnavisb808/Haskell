import Test.QuickCheck
prop_rev xs ys = reverse (xs ++ ys) == reverse xs ++ reverse ys
main = quickCheck prop_rev