type Cell = (Int,Int) 
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

delete ::  Cell -> [Cell] ->[Cell]
delete x [] = []
delete x (y:ys) | x == y = ys
				|otherwise = delete x ys 
up :: MyState -> MyState
up (S (x,y) l st ms)| x==0 = Null
					|otherwise = (S ((x-1),y) l "up" (S (x,y) l st ms))
								
down :: MyState -> MyState								
down (S (x,y) l st ms)| x==3 = Null
					  |otherwise = (S ((x+1),y) l "down" (S (x,y) l st ms))
left :: MyState -> MyState								
left (S (x,y) l st ms)| y==0 =  Null
					  |otherwise = (S (x,(y-1)) l "left" (S (x,y) l st ms))
right :: MyState -> MyState			
right (S (x,y) l st ms)| y==3 = Null
					   |otherwise = (S (x,(y+1)) l "right" (S (x,y) l "" ms))

member x [] = False
member x (y:ys) | x == y = True
				| otherwise = member x ys
				

collect :: MyState -> MyState
collect (S (x,y) l	st ms)| member (x,y) l = (S (x,y) (delete (x,y) l) "collect" (S (x,y) l st ms))
						  | otherwise =  Null
								
nextMyStates::MyState->[MyState]
nextMyStates ms = helper [] ms

insert x [] = [x]
insert x (y:ys) = (y : insert x ys)
helper l ms | (up(ms)/= Null) && (((member (up(ms)) l)) == False) = helper (insert (up ms) l) ms 
			| (down(ms)/= Null) && (((member (down(ms)) l) == False)) = helper (insert (down ms) l) ms
			| (left(ms)/= Null) && (((member (left(ms)) l) == False)) = helper (insert (left ms) l) ms
			| (right(ms)/= Null) && (((member (right(ms)) l) == False)) = helper (insert (right ms) l) ms 
			| otherwise = l
			
isGoal::MyState->Bool
isGoal (S (x,y) [] st ms) = True
isGoal (S (x,y) l st ms) = False
	
merge :: [a] -> [a] -> [a]
merge []     ys = ys
merge (x:xs) ys = x : (merge xs ys)
	
search::[MyState]->MyState
search (y:ys)| ((isGoal y) == True) = y 
			| otherwise = search (merge (nextMyStates y) ys)

constructSolution:: MyState ->[String]
constructSolution ms = helpercs [] ms 
helpercs res Null = res 
helpercs res (S pt l st ms) | st /= "" = helpercs (insert st res) ms 
							| otherwise = helpercs res ms 
		

solve :: Cell->[Cell]->[String]
solve (x,y) [] = []
solve (x,y) m = (constructSolution(search (nextMyStates(S (x,y) m "" Null))))
