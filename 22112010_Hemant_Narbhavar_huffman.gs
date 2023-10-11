--2) Project

ctype Tree where
	Node : Int -> Tree -> Tree -> Tree
	Etree : Tree

huffmanzip : String -> String
huffmanzip.n =  huffToS.n.(matchChar.(leafNodeHuff.(treeToDS.(freqToHuff.(freq.n)))).(freq.n))

----Frequency of element----
freq : String -> [(Int,Char)]
freq.[] = []
freq.n = sort.((count.n.(head.n),head.n) :: freq.(remoHead.n.(head.n)))

--count
count : String -> Char -> Int
count.[].n = 0
count.(x::xs).n = if x == n then 1 + count.xs.n else count.xs.n

--remove similer element like head from list
remoHead : String -> Char -> String
remoHead.[].n = []
remoHead.(x::xs).n = if x == n then remoHead.xs.n else x :: remoHead.xs.n


----Frequency sort list to Huffman Tree----
freqToHuff : [(Int,Char)] -> Tree
freqToHuff.[x] = Node.(value.x).Etree.Etree
freqToHuff.n = Node.(sumall.n).(Node.(highNum.n).Etree.Etree).(freqToHuff.(init.n))

--sum all frequency--
sumall : [(Int,Char)] -> Int
sumall.[] = 0
sumall.(x::xs) = value.x + sumall.xs

value.(x,y) = x

--highest Number--
highNum : [(Int,Char)] -> Int
highNum.[] = 0
highNum.(x::xs) = h3.xs.(value.x)

h3 : [(Int,Char)] -> Int -> Int
h3.[].n = n
h3.(x::xs).n = if n > value.x then h3.xs.n else h3.xs.(value.x)



----Convert Tree to DS----
treeToDS.(Node.n.Etree.Etree) = [("",n)]
treeToDS.(Node.n.Etree.b) = [("",n)] ++ (map.(\(a,b) -> ((::).'1'.a,b)).(treeToDS.b))
treeToDS.(Node.n.a.Etree) = [("",n)] ++ (map.(\(a,b) -> ((::).'0'.a,b)).(treeToDS.a))
treeToDS.(Node.n.a.b) = [("",n)] ++ (map.(\(a,b) -> ((::).'0'.a,b)).(treeToDS.a)) ++ (map.(\(a,b) -> ((::).'1'.a,b)).(treeToDS.b))


----LeafNode of Huffman Tree List----
leafNodeHuff : [(String,Int)] -> [(String,Int)]
leafNodeHuff.[] = []
leafNodeHuff.[x] = [x]
leafNodeHuff.[x,y] = x :: y :: []
leafNodeHuff.(x::y::xs) = y :: leafNodeHuff.xs

--leaf Nodes List--
leafNode : Tree -> [Int]
leafNode.(Node.x.Etree.Etree) = [x]
leafNode.(Node.x.y.Etree) = leafNode.y
leafNode.(Node.x.Etree.z) = leafNode.z
leafNode.(Node.x.y.z) = leafNode.y ++ leafNode.z

--Match with Character--
matchChar : [(String,Int)] -> [(Int,Char)] -> [(String,Char)]
matchChar.n.m = matchChar1.n.(reverse.m)
matchChar1 : [(String,Int)] -> [(Int,Char)] -> [(String,Char)]
matchChar1.[].[] = []
matchChar1.(x::xs).(y::ys) = (toString.x, toChar.y) :: matchChar1.xs.ys

toString.(x,y) = x
toChar.(x,y) = y

----Huffman List to binary String----
huffToS : String -> [(String,Char)] -> String
huffToS.[].n = []
huffToS.(x::xs).n =  huffToS1.x.n.xs ++ huffToS.xs.n

huffToS1 : Char -> [(String,Char)] -> String -> String
huffToS1.x.(y::ys).xs = if x == toChar.y then toString.y else huffToS1.x.ys.xs






