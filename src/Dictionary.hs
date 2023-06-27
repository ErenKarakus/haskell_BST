module Dictionary (createDict, sizeDict, lookupDict, insertDict, removeDict, removeIfDict, displayDict, displayListDict) where
import BST

createDict = BST.createBST
sizeDict dictionary = BST.size dictionary
lookupDict key dictionary = BST.lookupBST key dictionary
insertDict key item dictionary = BST.insert key item dictionary
removeDict key dictionary = BST.remove key dictionary
removeIfDict condition dictionary = BST.removeIf condition dictionary
displayDict dictionary = BST.display dictionary
displayListDict dictionary = BST.displayList dictionary
