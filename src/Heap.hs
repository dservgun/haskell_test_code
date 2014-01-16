module Heap where
class Heap h where
    empty :: Ord a => h a
    isEmpty :: Ord a => h a -> Bool
    insert :: Ord a => a -> h a -> h a
    merge :: Ord a => h a -> h a -> h a
    findMin :: Ord a => h a -> a
    deleteMin :: Ord a => h a -> h a
    
listHeap :: (Ord a, Heap h) => [a] -> h a    
listHeap = foldl (\h a -> insert a h) empty

heapList :: (Ord a, Heap h) => h a -> [a]
heapList h = if isEmpty h then [] else findMin h : heapList (deleteMin h)

type Rank = Int    
data LeftistHeap a = E | T Rank a (LeftistHeap a) (LeftistHeap a)
    deriving (Show, Eq)

rank :: LeftistHeap a -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: a -> LeftistHeap a -> LeftistHeap a -> LeftistHeap a
makeT x a b = if rank a >= rank b
                then T (rank b + 1) x a b
                else T (rank a + 1) x b a
instance Heap LeftistHeap where
    empty = E 
    isEmpty = (==E)
    insert x = merge (T 1  x E E) 
    merge h E = h
    merge E h = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
            if x <= y 
                then makeT  x a1 (merge b1 h2)
                else makeT y a2 (merge h2 b2)
    findMin E = error "Empty heap"
    findMin (T _ x _ _) = x

    deleteMin E = error "Empty heap"
    deleteMin (T _ _ a b) = merge a b