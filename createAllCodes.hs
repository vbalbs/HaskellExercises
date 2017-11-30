createAllCodes 0 = [[]]
createAllCodes length = [ c:r | c <- [1,2,3,4,5,6],  r <- createAllCodes (length-1), notElem c r ]
