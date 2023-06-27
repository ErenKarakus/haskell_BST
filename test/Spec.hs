import Test.HUnit
import Lib  
import BST
import BST_tests
import Dictionary
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.List(sort, nubBy)

import Data.Function ( on )


import Debug.Trace

main :: IO ()
main = do
  lookupResults <- runTestTT lookupTests
  insertResults <- runTestTT insertTests
  removeResults <- runTestTT removeTests
  removeIfReults <- runTestTT removeIfTests
  displayListReults <- runTestTT displayListTests
  sizeReults <- runTestTT sizeTests
  --rotateLeftReults <- runTestTT rotateLeftTests
  --rotateRightResults <- runTestTT rotateRightTests
  runDictTests
  run_group
  quickCheck prop_insert
  quickCheck prop_size
  quickCheck prop_test_removal
  quickCheck prop_lookup_after_insert
  quickCheck prop_size_increase_by_one
  quickCheck prop_size_decrease_by_one
  quickCheck prop_displays_keys_in_order

  return ()

lookupTests :: Test
lookupTests = TestList [    
  TestCase (assertEqual "Lookup Test" (Just "David") (lookupBST 5 constructorBST)),
  TestCase (assertEqual "Root Node" (Just "James") (lookupBST 20 constructorBST)),
  TestCase (assertEqual "Not in Tree" Nothing (lookupBST 33 constructorBST)),
  TestCase (assertEqual "Lookup Test polymorphic" (Just "David") (lookupBST "D" constructorBST2)),
  TestCase (assertEqual "Root Node polymorphic" (Just "James") (lookupBST "J" constructorBST2)),
  TestCase (assertEqual "Not in Tree polymophic" Nothing (lookupBST "Z" constructorBST2))
  ]

insertTests :: Test
insertTests = TestList [
  TestCase (assertEqual "New node right sub-tree" (Just "Testing") (lookupBST 50 (insert 50 "Testing" constructorBST))),
  TestCase (assertEqual "New node left sub-tree" (Just "SmallestKey") (lookupBST 3 (insert 3 "SmallestKey" constructorBST))),
  TestCase (assertEqual "Replace existing node" (Just "New Thomas") (lookupBST 15 (insert 15 "New Thomas" constructorBST))),
  TestCase (assertEqual "Insert empty" (Just "First") (lookupBST 5 (insert 5 "First" createBST))),
  TestCase (assertEqual "Full Tree Test" insertBST26 (insert 26 "insertTest" insertBST26)),
  TestCase (assertEqual "Replace existing node String" (Just "New Thomas") (lookupBST "T" (insert "T" "New Thomas" constructorBST2)))
  ]

removeTests :: Test
removeTests = TestList [
    TestCase (assertEqual "Remove node with no children" removeNoChildBST (remove 2 constructorBST)),
    TestCase (assertEqual "Remove node with only left child" removeLeftChildBST (remove 5 constructorBST)),
    TestCase (assertEqual "Remove node with only right child" removeRightChildBST (remove 15 constructorBST)),
    TestCase (assertEqual "Remove node with two children" removeTwoChildBST (remove 30 constructorBST)),
    TestCase (assertEqual "Remove non existing node" constructorBST (remove 120 constructorBST)),
    TestCase (assertEqual "Remove node with two children, polymorphic" removeTwoChildBST2 (remove "M" constructorBST2))
  ]

isEven :: Int -> Bool
isEven n = n `mod` 2 == 0

isOdd :: Int -> Bool
isOdd n = n `mod` 2 /= 0

removeIfTests :: Test
removeIfTests = TestList [
  TestCase (assertEqual "Given Leaf returns Leaf" Leaf (removeIf isEven emptyBST)),
  TestCase (assertEqual "Remove Evens" (Node 25 "Sarah" (Node 15 "Thomas" (Node 5 "David" Leaf Leaf) (Node 17 "ThomasChild" Leaf Leaf)) (Node 35 "Random" Leaf Leaf)) (removeIf isEven constructorBST)),
  TestCase (assertEqual "Remove Odds" (Node 20 "James" (Node 10 "Robert" (Node 2 "DavidChild" Leaf Leaf) Leaf) (Node 30 "Mary" Leaf Leaf)) (removeIf isOdd constructorBST))
  ]

displayListTests :: Test
displayListTests = TestList [
  TestCase (assertEqual "List Empty Tree" [] (displayList emptyBST)),
  TestCase (assertEqual "List tree with single node" [(5, "single_node")] (displayList (insert 5 "single_node" createBST))),
  TestCase (assertEqual "List constructor" constructorBSTList (displayList constructorBST)),
  TestCase (assertEqual "List constructor polymorphic" constructorBST2List (displayList constructorBST2))
  ]

sizeTests :: Test
sizeTests = TestList [
  TestCase (assertEqual "Size of empty tree" 0 (size createBST)),
  TestCase (assertEqual "Size of constructorBST" 9 (size constructorBST)),
  TestCase (assertEqual "Size of constructorBST2" 7 (size constructorBST2))
  ]

rotateRightTests :: Test
rotateRightTests = TestList [
  TestCase (assertEqual "rotateRight on Leaf" (Leaf :: BST Int String) (rotateRight Leaf)),
  TestCase (assertEqual "rotateRight on a Node with no right subtree" removeLeftChildBST (rotateRight removeLeftChildBST :: BST Int String)),
  TestCase (assertEqual "rotateRight on a Node with a right subtree" removeTwoChildBST (rotateRight removeTwoChildBST :: BST Int String))
  ]

rotateLeftTests :: Test
rotateLeftTests = TestList [
  TestCase ( assertEqual "Rotate left test"
                    (Node 30 "Mary" (Node 20 "James" (Node 10 "Robert" (Node 5 "David" (Node 2 "DavidChild" Leaf Leaf) (Node 8 "New" Leaf Leaf)) 
                     (Node 15 "Thomas" Leaf (Node 17 "ThomasChild" Leaf Leaf))) 
                      (Node 25 "Sarah" Leaf Leaf)) (Node 35 "Random" Leaf Leaf))
                    (rotateLeft $ insert 8 "New" constructorBST))
  ]
                   

runDictTests :: IO ()
runDictTests = do
  dictionaryResults <- runTestTT dictionaryTests
  return ()

dictionaryTests :: Test
dictionaryTests = TestList [
  TestCase (assertEqual "Dictionary lookup Test" (Just "David") (Dictionary.lookupDict 5 constructorBST)),
  TestCase (assertEqual "Dictionary replace existing node" (Just "New Thomas") (Dictionary.lookupDict 15 (insert 15 "New Thomas" constructorBST))),
  TestCase (assertEqual "Dictionary remove node with two children" removeTwoChildBST (Dictionary.removeDict 30 constructorBST)),
  TestCase (assertEqual "Dictionary remove Evens" (Node 25 "Sarah" (Node 15 "Thomas" (Node 5 "David" Leaf Leaf) (Node 17 "ThomasChild" Leaf Leaf)) (Node 35 "Random" Leaf Leaf)) (Dictionary.removeIfDict isEven constructorBST)),
  TestCase (assertEqual "Dictionary list constructor polymorphic" constructorBST2List (Dictionary.displayListDict constructorBST2))
  ]

---------------

-- instance (Arbitrary key, Arbitrary item) => Arbitrary (BST key item) where
--   arbitrary = sized genTree

-- genTree n 
--   | n > 0 = do
--     key <- arbitrary
--     item <- arbitrary
--     leftChild <- genTree (n `div` 2)
--     rightChild <- genTree (n `div` 2)
--     return (Node key item leftChild rightChild)
--   | otherwise = return Leaf

instance (Ord key, Arbitrary key, Arbitrary item) => Arbitrary (BST key item) where
    arbitrary = sized $ \n -> do 
        testing <- listOf arbitrary 
        return (foldl (flip $ uncurry BST.insert) Leaf testing)


prop_insert :: Int -> String -> BST Int String -> Bool
prop_insert key item tree  = lookupBST key (insert key item tree) == Just item

populateBSTFromPairs :: (Ord key) => [(key, item)] -> BST key item
populateBSTFromPairs = foldr (\(key, value) tree -> insert key value tree) Leaf

prop_size :: [(Int, Int)] -> Bool
prop_size pairs = do
    let tree = populateBSTFromPairs pairs in
        let expectedSize = length (nubBy ((==) `on` fst) pairs) in
            expectedSize == size tree 

prop_test_removal :: Int -> String -> BST Int String -> Bool
prop_test_removal removeKey value tree = do 
    let altered_bst = insert removeKey value tree
    Nothing == lookupBST removeKey (remove removeKey altered_bst)


prop_lookup_after_insert :: Int -> String -> BST Int String -> Bool
prop_lookup_after_insert key item tree = 
  lookupBST key (insert key item tree) == Just item


prop_size_increase_by_one :: Int -> String -> BST Int String -> Bool
prop_size_increase_by_one key item tree = --trace ("displayList: " ++ show tree) $
  if lookupBST key tree == Nothing
    then size tree + 1 == size altered_bst && lookupBST key altered_bst == Just item
    else size tree == size altered_bst
  where altered_bst = insert key item tree

prop_size_decrease_by_one :: Int -> String -> BST Int String -> Bool
prop_size_decrease_by_one key item tree = --trace ("displayList: " ++ show tree) $
  let altered_bst = remove key (insert key item tree)
  in (size tree == size altered_bst) || (size tree - 1 == size altered_bst)

size_increase_decrease :: TestTree
size_increase_decrease = testGroup "increase_decrease_size"
  [
    testProperty "increase size by at most one per insert" prop_size_increase_by_one,
    testProperty "decrease size by at most one per removal" prop_size_decrease_by_one
  ]

run_group :: IO ()
run_group = defaultMain size_increase_decrease

prop_displays_keys_in_order :: Int -> String -> Bool
prop_displays_keys_in_order key item = --trace ("displayList: " ++ show key) $
  let altered_bst = insert key item constructorBST in
    displayListKeys (displayList altered_bst) == sort(displayListKeys(displayList altered_bst))


prop_rotateRightPreservesItems :: (Ord key, Eq item, Show key, Show item) => BST key item -> Bool
prop_rotateRightPreservesItems tree = --trace ("displayList: " ++ show tree) $
    let rotated = rotateRight tree
        original = findItems (getKeys tree) tree
        rotatedItems = findItems (getKeys rotated) rotated
    in original == rotatedItems
    where
        getKeys Leaf = []
        getKeys (Node key _ left right) = getKeys left ++ [key] ++ getKeys right
        
        search Leaf _ = Nothing
        search (Node key item left right) k
            | key == k = Just item
            | key > k = search left k
            | otherwise = search right k
        
        findItems [] _ = []
        findItems (key:keys) t = case search t key of
            Nothing -> error "Key not found"
            Just item -> item : findItems keys t

prop_rotateRightPreservesKeys :: (Ord key, Show key, Show item) => BST key item -> Bool
prop_rotateRightPreservesKeys tree = --trace ("displayList: " ++ show tree) $
    let rotatedKeys = sort $ getKeys (rotateRight tree)
    in rotatedKeys == sort (getKeys tree)
    where
        getKeys Leaf = []
        getKeys (Node key _ left right)
            = getKeys left ++ [key] ++ getKeys right