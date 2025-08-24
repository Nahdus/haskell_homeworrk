
joinMoves :: [(Peg,Peg)] -> [(Peg,Peg)] ->[(Peg,Peg)]
joinMoves [] y = y
joinMoves (x:[]) y = x:y
joinMoves (x:x2:(zs)) y = x:x2:(joinMoves zs y)



type Peg = String
type Move = (Peg, Peg)
hanoi::Integer -> Peg ->Peg -> Peg -> [Move]
hanoi 0 x y z = []
hanoi 1 x y z = [(x,z)]
hanoi n x y z = joinMoves (hanoi (n-1) x z y) (joinMoves [(x,z)] (hanoi (n-1) y x z))
-- hanoi 2 x y z = [(x,y),(x,z),(y,z)]




