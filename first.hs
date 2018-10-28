f [] = 0
f (h:tail) = 1 + f tail

make_positive [] = []
make_positive (h:tail) = if (h < 0) then (h * (-1)):make_positive tail else h:make_positive tail

absolut x | x >= 0 = x
	      |x < 0 = -x

mmax x y | x < y = y
		 | x >= y = x
		 
qsort [] = []
qsort (h:tail) = qsort [x | x <- tail, x < h] ++ [h] ++ qsort [x | x <- tail, x >= h]

data Tree = Empty | Node Int Tree Tree deriving Show

node_count Empty = 0
node_count (Node x left right) = 1 + node_count left + node_count right

search_tree (key, Empty) = False
search_tree (key, (Node x left right)) = key == x || search_tree(key, left) || search_tree(key, right)

add_element (elem, Empty) = Node elem Empty Empty
add_element (elem, (Node x left right)) = if elem < x then Node x (add_element(elem, left)) right else if elem > x then Node x left (add_element(elem, right)) else Node x left right

max_element (Node x Empty Empty) = x
max_element (Node x left right) = max_element right

del_element (elem, (Node x Empty Empty)) = if elem == x then Empty else Node x Empty Empty
del_element (elem, (Node x left right)) = 	if elem < x then 
												Node x (del_element(elem, left)) right 
											else if elem > x then 
												Node x left (del_element(elem, right)) 
											else 
												let 
													max_left = max_element left
												in
													Node max_left (del_element(max_left, left)) right