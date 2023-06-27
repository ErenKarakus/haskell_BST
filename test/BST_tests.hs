module BST_tests where
import BST

constructorBST :: BST Int String
constructorBST =
    Node 20 "James" 
        (Node 10 "Robert" (Node 5 "David" (Node 2 "DavidChild" Leaf Leaf) Leaf) (Node 15 "Thomas" Leaf(Node 17 "ThomasChild" Leaf Leaf)) )
        (Node 30 "Mary" (Node 25 "Sarah" Leaf Leaf) (Node 35 "Random" Leaf Leaf))

constructorBST2 :: BST String String
constructorBST2 = 
    Node "J" "James"
        (Node "R" "Robert" (Node "D" "David" Leaf Leaf) (Node "T" "Thomas" Leaf Leaf) )
        (Node "M" "Mary" (Node "S" "Sarah" Leaf Leaf) (Node "R" "Random" Leaf Leaf))

insertBST26 ::  BST Int String
insertBST26 = 
    Node 20 "James"
        (Node 10 "Robert" (Node 5 "David" Leaf Leaf) (Node 15 "Thomas" Leaf (Node 17 "ThomasChild" Leaf Leaf)) )
        (Node 30 "Mary" (Node 25 "Sarah" Leaf (Node 26 "insertTest" Leaf Leaf)) (Node 35 "Random" Leaf Leaf))

removeNoChildBST :: BST Int String
removeNoChildBST =
  Node 20 "James"
    (Node 10 "Robert" (Node 5 "David" Leaf Leaf) (Node 15 "Thomas" Leaf (Node 17 "ThomasChild" Leaf Leaf)) )
    (Node 30 "Mary" (Node 25 "Sarah" Leaf Leaf) (Node 35 "Random" Leaf Leaf))

removeLeftChildBST :: BST Int String
removeLeftChildBST =
  Node 20 "James"
    (Node 10 "Robert" (Node 2 "DavidChild" Leaf Leaf) (Node 15 "Thomas" Leaf (Node 17 "ThomasChild" Leaf Leaf)) )
    (Node 30 "Mary" (Node 25 "Sarah" Leaf Leaf) (Node 35 "Random" Leaf Leaf))

removeRightChildBST :: BST Int String
removeRightChildBST =
  Node 20 "James"
    (Node 10 "Robert" (Node 5 "David" (Node 2 "DavidChild" Leaf Leaf) Leaf) (Node 17 "ThomasChild" Leaf Leaf) ) 
    (Node 30 "Mary" (Node 25 "Sarah" Leaf Leaf) (Node 35 "Random" Leaf Leaf))

removeTwoChildBST :: BST Int String
removeTwoChildBST =
  Node 20 "James"
    (Node 10 "Robert" (Node 5 "David" (Node 2 "DavidChild" Leaf Leaf) Leaf) (Node 15 "Thomas" Leaf (Node 17 "ThomasChild" Leaf Leaf)) )
    (Node 35 "Random" (Node 25 "Sarah" Leaf Leaf) Leaf)

removeTwoChildBST2 :: BST String String
removeTwoChildBST2 =
  Node "J" "James"
    (Node "R" "Robert" (Node "D" "David" Leaf Leaf) (Node "T" "Thomas" Leaf Leaf) )
    (Node "R" "Random" (Node "S" "Sarah" Leaf Leaf) Leaf )

emptyBST :: BST Int String
emptyBST = Leaf

constructorBSTList :: [(Int, String)]
constructorBSTList = [(2,"DavidChild"),(5,"David"),(10,"Robert"),(15,"Thomas"),(17,"ThomasChild"),(20,"James"),(25,"Sarah"),(30,"Mary"),(35,"Random")]

constructorBST2List :: [(String, String)]
constructorBST2List = [("D","David"),("R","Robert"),("T","Thomas"),("J","James"),("S","Sarah"),("M","Mary"),("R","Random")]