findK :: Int -> [a] -> a
findK 0 (x:_) = x
findK k [] = error "Erro"
findK k (x:xs) = findK (k-1) xs
