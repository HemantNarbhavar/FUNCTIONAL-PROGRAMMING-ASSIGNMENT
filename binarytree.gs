ctype Tree where
	Node: Int -> Tree -> Tree -> Tree
	Etree: Tree

--MIRROR
mirrorTree.(Node.n.Etree.Etree) = Node.n.Etree.Etree
mirrorTree.(Node.n.Etree.b) = Node.n.(mirrorTree.b).Etree
mirrorTree.(Node.n.a.Etree) = Node.n.Etree.(mirrorTree.a)
mirrorTree.(Node.n.a.b) = Node.n.(mirrorTree.b).(mirrorTree.a)

--HIGHEST LAYER
addL.[].[] = []
addL.[].b = b
addL.a.[] = a
addL.(x::xs).(y::ys) = x+y :: addL.xs.ys

hL.(Node.n.Etree.Etree) = [1]
hL.(Node.n.Etree.b) = [1] ++ addL.[].(hL.b)
hL.(Node.n.a.Etree) = [1] ++ addL.(hL.a).[]
hL.(Node.n.a.b) = [1] ++ addL.(hL.a).(hL.b)

maxPos.[(a,b)] = b
maxPos.((a,b) :: (x,y) :: xs) = if x < a then maxPos.((a,b) :: xs) else maxPos.((x,y) :: xs)

highestLayer.(Node.n.a.b) = maxPos.(zip.(hL.(Node.n.a.b)).[1...])

--SUM TREE
sumTree.(Node.n.Etree.Etree)=n
sumTree.(Node.n.Etree.b)=n+sumTree.b
sumTree.(Node.n.a.Etree)=n+sumTree.a
sumTree.(Node.n.a.b)=n+sumTree.a+sumTree.b

--LEAF NODE
leafNode.(Node.n.Etree.Etree)=[n]
leafNode.(Node.n.Etree.b)=leafNode.b
leafNode.(Node.n.a.Etree)=leafNode.a
leafNode.(Node.n.a.b)=leafNode.a++leafNode.b

--GREATER THAN NODE
gtn.(Node.n.Etree.Etree) = []
gtn.(Node.n.Etree.(Node.a.b.c)) = if a > n then a :: gtn.(Node.a.b.c) else gtn.(Node.a.b.c)
gtn.(Node.n.(Node.a.b.c).Etree) = if a > n then a :: gtn.(Node.a.b.c) else gtn.(Node.a.b.c)
gtn.(Node.n.(Node.a.b.c).(Node.p.q.r)) = if (a > n) && (p > n) then a :: p :: (++).(gtn.(Node.a.b.c)).(gtn.(Node.p.q.r)) else if (a > n) then a :: (++).(gtn.(Node.a.b.c)).(gtn.(Node.p.q.r)) else if (p > n) then p :: (++).(gtn.(Node.a.b.c)).(gtn.(Node.p.q.r)) else (++).(gtn.(Node.a.b.c)).(gtn.(Node.p.q.r))

-- CONVERTER
insertL.l = map.(\(a, b) -> ('L' :: a, b)).l
insertR.l = map.(\(a, b) -> ('R' :: a, b)).l

convert.(Node.a.Etree.Etree) = [("", a)]
convert.(Node.a.Etree.b) = [("", a)] ++ insertR.(convert.b)
convert.(Node.a.b.Etree) = [("", a)] ++ insertL.(convert.b)
convert.(Node.a.b.c) = [("", a)] ++ insertL.(convert.b) ++ insertR.(convert.c)