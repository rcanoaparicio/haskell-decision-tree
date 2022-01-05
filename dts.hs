data MushroomType = None | Edible | Poisonous deriving (Show)

-- Tree: Node "attribute-name" MushroomType [Children]
data Tree a = Node String MushroomType [Tree a] | Empty deriving (Show)

-- Attribute: Attribute "column" [posible values] "Dictionary of the ocurrences"
data Attribute a = Attribute Int [String] Dict


-- poisonous: adds an edible to the Attribute
poisonous :: Attribute a -> String -> Attribute a
poisonous (Attribute n l d) ns = (Attribute n l nd)
  where 
    nd = insert d ns (fst x + 1, snd x)
    x  = search d ns 

-- edible: adds an edible to the Attribute
edible :: Attribute a -> String -> Attribute a
edible (Attribute n l d) ns = (Attribute n l nd)
  where 
    nd = insert d ns (fst x, snd x + 1)
    x  = search d ns 

-- getInfo: returns the values of the dictionary in a list
getInfo :: Attribute a -> [(String, (Int, Int))]
getInfo (Attribute n [] d)     = []
getInfo (Attribute n (x:xs) d) = (x, search d x) : getInfo (Attribute n xs d) 


-- Dictionary (key: String, value: Int tuple)
type Dict = (String -> (Int, Int))
create :: (Int, Int) -> Dict
create = const
search :: Dict -> String -> (Int, Int)
search dict key = dict key
insert :: Dict -> String -> (Int, Int) -> Dict
insert dict key value x
  | key == x  = value
  | otherwise = search dict x

-- splot: splits a string for a given character
split :: String -> Char -> [String]
split [] _ = []
split s c  = [a] ++ split b c
  where
    a = takeWhile (/= c) $ dropWhile (== c) s
    b = dropWhile (/= c) $ dropWhile (== c)  s

-- process: splits each line of the input to prepare the data for its usage
process :: [String] -> [[String]]
process d = map split' d
  where 
    split' a = split a ',' 

-- calcAttributes: calculates the ocurrences of poisonous|edible for each attribute
calcAttributes :: [[String]] -> [Attribute a]
calcAttributes (d:[]) = map calcAttributes' (tail d)
  where 
    calcAttributes' s
      | head d == "p" = Attribute 0 [s] (insert (create (0, 0)) s (1, 0))
      | otherwise     = Attribute 0 [s] (insert (create (0, 0)) s (0, 1))
calcAttributes (d:ds) = zipWith calcAttributes' (tail d) e
  where
    e = calcAttributes ds
    calcAttributes' a (Attribute _ l di)
      | head d == "p" = Attribute 0 nl (insert di a (fst ne + 1, snd ne))
      | otherwise     = Attribute 0 nl (insert di a (fst ne, snd ne + 1))
        where
          nl = insertInList l a
          ne = search di a

-- insertInList: inserts an element in a list if it doesn't exist
insertInList :: Eq a => [a] -> a -> [a]
insertInList [] n     = [n]
insertInList (x:xs) n
  | x == n    = x : xs
  | otherwise = x : insertInList xs n


-- calcInfo: evaluates the precision for each of the attributes
calcInfo :: [Attribute a] -> [Int]
calcInfo []                      = []
calcInfo ((Attribute _ ns d):xs) = (calcInfo' ns) : calcInfo xs
  where
    calcInfo' :: [String] -> Int
    calcInfo' []     = 0
    calcInfo' (y:ys) = max (fst z) (snd z) + calcInfo' ys
      where z = search d y

-- maxIndex: returns the index of the first occurence of the maximum value in a list
maxIndex :: [Int] -> Int
maxIndex l = maxIndex' l 0
  where
    mn = maximum l
    maxIndex' [] _ = 0
    maxIndex' (x:xs) n
      | x == mn   = n
      | otherwise = maxIndex' xs (n + 1)

-- filterTable: filters the dataset for a value of a given property
filterTable :: [[String]] -> Int -> String -> [[String]]
filterTable s i a = dropColumn [x | x <- s, (x !! (i+1)) == a]
  where 
    dropColumn []     =  []
    dropColumn (x:xs) = [l] ++ dropColumn xs
      where l = take (i + 1) x ++ drop (i + 2) x


-- buildTree: creates the children of the decision-tree
buildTree :: [[String]] -> (String, (Int, Int)) -> Tree a
buildTree [] _ = Empty
buildTree d i
  | (fst $ snd i) == 0 = Node (fst i) Edible []
  | (snd $ snd i) == 0 = Node (fst i) Poisonous []
  | otherwise          = Node (fst i) None ch
    where 
      t = getInfo $ att !! (maxIndex info)
      info = calcInfo att
      att = calcAttributes d
      ch = map buildTree' t
        where 
          buildTree' :: (String, (Int, Int)) -> Tree a
          buildTree' e = buildTree (filterTable d (maxIndex info) (fst e)) e


-- createTree: creates a decision-tree for a given data matrix (first call)
createTree :: [[String]] -> Tree a
createTree d = Node "root" None ch
  where
    t = getInfo $ att !! (maxIndex info)
    info = calcInfo att
    att = calcAttributes d
    ch = map createTree' t
      where 
        createTree' :: (String, (Int, Int)) -> Tree a
        createTree' e = buildTree (filterTable d (maxIndex info) (fst e)) e

-- toString: given a tree and a "prefix", returns a string-visualization of the tree
toString :: Tree a -> String -> String
toString (Node n Edible []) p    = p ++ n ++ "(e)\n"
toString (Node n Poisonous []) p = p ++ n ++ "(p)\n"
toString (Node n t c) p          = p ++ n ++ "\n" ++ toString' c
  where
    toString' []     = []
    toString' (x:xs) = toString x np ++ toString' xs
      where 
        np = aux p ++ "\\__"
          where
            aux [] = []
            aux st = "|  " ++ aux (drop 3 st)


-- getQuestion: returns a string with the options to navigate to the next level
--              of the tree
getQuestion :: Tree a -> String
getQuestion (Node _ Poisonous _) = "NO MORE QUESTIONS"
getQuestion (Node _ Edible _)    = "NO MORE QUESTIONS"
getQuestion (Node n None c)      = drop 1 (getQuestion' c)
  where 
    getQuestion' [] = []
    getQuestion' ((Node x _ _):xs) = "," ++ x ++ getQuestion' xs



-- goToChild: given a tree and the name of one of its children, returns the children
goToChild :: Tree a -> String -> Tree a
goToChild (Node _ _ c) n = goToChild' c
  where
    goToChild' [] = Empty
    goToChild' ((Node a b c):xs)
      | a == n    = (Node a b c)
      | otherwise = goToChild' xs


-- isValidChild: checks if a String is the name of one of the Tree's childen
isValidChild :: [Tree a] -> String -> Bool
isValidChild [] s = False
isValidChild ((Node n _ _):xs) s
  | n == s    = True
  | otherwise = isValidChild xs s


-- readInput: receives a tree and lets the user navigate through one of the
--            children of the root
readInput :: Tree a -> IO ()
readInput (Node _ Edible _) = do
  putStrLn "Edible"
readInput (Node _ Poisonous _) = do
  putStrLn "Poisonous"
readInput (Node n t c) = do
  putStrLn $ toString (Node n t c) ""
  putStrLn $ "<system> " ++ getQuestion (Node n t c) ++ "?"
  user <- getLine
  if isValidChild c user then
    readInput (goToChild (Node n t c) user)
  else do
    putStrLn "Invalid option"
    readInput (Node n t c)



-------------------
-- Main function --
-------------------
main :: IO ()
main = do
  contents <- readFile "agaricus-lepiota.data"
  let dataset = lines contents
  let d = process dataset
  let t = createTree d
  putStrLn "###################################\n##### Mushrooms Decision Tree #####\n###################################"
  readInput t
