duplicate [] = []
duplicate [x] = [x,x]
duplicate xs = (([head xs, head xs]) ++ (duplicate (tail xs))) 
