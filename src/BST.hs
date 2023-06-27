module BST where

data BST key item = 
    Node key item (BST key item) (BST key item)
    | Leaf
    deriving (Show, Eq)

-- create a new BST
createBST :: BST key item
createBST = Leaf

-- return the number of nodes in the BST
size :: BST key item -> Int
size Leaf = 0
size (Node key item leftChild rightChild) = 1 + size leftChild + size rightChild

-- searches a tree for a node with the given key
lookupBST :: (Ord key) => key -> BST key item -> Maybe item
lookupBST soughtKey Leaf = Nothing
lookupBST soughtKey (Node key item leftChild rightChild) 
    | soughtKey < key = lookupBST soughtKey leftChild
    | soughtKey > key = lookupBST soughtKey rightChild
    | otherwise = Just item

-- inserts a node into the BST
insert :: (Ord key) => key -> item -> BST key item -> BST key item
insert insertKey insertValue Leaf = Node insertKey insertValue Leaf Leaf
insert insertKey insertValue (Node key value leftChild rightChild) 
    | insertKey == key = Node key insertValue leftChild rightChild
    | insertKey > key = Node key value leftChild (insert insertKey insertValue rightChild)
    | insertKey < key = Node key value (insert insertKey insertValue leftChild) rightChild

-- finds the node to be removed from a BST
remove :: (Ord key) => key -> BST key item -> BST key item
remove removeKey Leaf = Leaf
remove removeKey (Node key item leftChild rightChild)
    | removeKey == key = removeNode (Node key item leftChild rightChild)
    | removeKey > key = Node key item leftChild (remove removeKey rightChild)
    | removeKey < key = Node key item (remove removeKey leftChild) rightChild
    

-- removes a node from the BST
removeNode :: (Ord key) => BST key item -> BST key item
removeNode (Node key item Leaf Leaf) = Leaf
removeNode (Node key item Leaf rightChild) = rightChild
removeNode (Node key item leftChild Leaf) = leftChild
removeNode (Node key item leftChild rightChild) = (Node key2 item2 leftChild (remove key2 rightChild))
    where (key2, item2) = findMinimumNode rightChild

-- finds the minimum node in a BST
findMinimumNode :: (Ord key) => BST key item -> (key, item)
findMinimumNode (Node key item Leaf _) = (key, item)
findMinimumNode (Node key item leftChild _) = findMinimumNode leftChild

-- removes nodes from the BST
removeIf :: (Ord key) => (key -> Bool) -> BST key item -> BST key item
removeIf condition Leaf = Leaf
removeIf condition (Node key item leftChild rightChild) =
    if condition key
        then removeIf condition (remove key (Node key item leftChild rightChild))
        else Node key item (removeIf condition leftChild) (removeIf condition rightChild)

-- display key and items of BST in order
display :: (Ord key, Show key, Show item) => BST key item -> IO()
display Leaf = return ()
display (Node key item leftChild rightChild) = do
    display leftChild
    putStrLn (show key ++ " " ++ show item)
    display rightChild

--display list of keys and value pairs in order
displayList :: BST key item -> [(key, item)]
displayList Leaf = []
displayList (Node key value Leaf Leaf) = [(key, value)]
displayList (Node key value leftChild rightChild) = 
    displayList leftChild ++ [(key, value)] ++ displayList rightChild

-- get keys returned by displayList
displayListKeys :: [(key, item)] -> [key]
displayListKeys list = map fst list

rotateRight :: BST key item -> BST key item
rotateRight Leaf = Leaf
rotateRight (Node key item leftChild (Node key2 item2 alpha beta)) = Node key2 item2 (Node key item leftChild alpha) beta
rotateRight tree = tree

rotateLeft :: BST key item -> BST key item
rotateLeft (Node key1 item1 alpha (Node key2 item2 beta gamma))
    = Node key2 item2 (Node key1 item1 alpha beta) gamma


