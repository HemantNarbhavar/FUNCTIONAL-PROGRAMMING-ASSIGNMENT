ctype Exp where 
	Val : String -> Exp
	SubExp : Op -> Exp -> Exp -> Exp
	Bracket : Exp -> Exp

ctype Op where
	Plus , Minus, Div, Mul, Exponent, Modulo : Op

optostr.Plus ="+"
optostr.Minus ="-"
optostr.Mul	="*"
optostr.Div	="/"
optostr.Exponent = "^"
optostr.Modulo = "%"
strtoop."+"	=Plus
strtoop."-"	=Minus
strtoop."*"	=Mul
strtoop."/"	=Div
strtoop."^" = Exponent
strtoop."%" = Modulo


----Simple Evaluator---

complexEval : String -> Int
complexEval.n = simpleeval.(parser.n)

-- Parser String to Expression
parser : String -> Exp
parser.n = parser1.(slist.n)

parser1 : [String] -> Exp
parser1.n = parser2.(reverse.n)

parser2 : [String] -> Exp
parser2.[n] = Val.n
parser2.(x::y::xs) = SubExp.(convt.(y)).(Val.x).(parser2.xs)

convt : String -> Op
convt.n = if n == "+" then Plus else if n == "-" then Minus else if n == "*" then Mul else Div

--String to List of elements of String

firstele : String -> String
firstele.[] = []
firstele.(x::xs) = if ord.x > 47 && ord.x < 58 then x :: firstele.xs else []

remfirstele : String -> String
remfirstele.[] = []
remfirstele.(x::xs) = if ord.x > 39 && ord.x < 48 then xs else remfirstele.xs

op1 : String -> [Char]
op1.[] = []
op1.(x::xs) = if ord.x > 39 && ord.x < 48 then [x] else op1.xs


slist : String -> [String]
slist.[] = []
slist.l = init.(slist1.l)

slist1 : String -> [String]
slist1.[] = []
slist1.n = firstele.n :: op1.n :: slist1.(remfirstele.n)

simpleeval : Exp -> Int
simpleeval.(Val.a) = convEI.(Val.a)
simpleeval.(SubExp.Plus.a.b) = convEI.a + simpleeval.b
simpleeval.(SubExp.Minus.a.b) = simpleeval.b - convEI.a
simpleeval.(SubExp.Mul.a.b) = convEI.a * simpleeval.b
simpleeval.(SubExp.Div.a.b) = simpleeval.b / convEI.a 

convEI : Exp -> Int
convEI.(Val.a) = strtoint.a

----BODMAS Evaluator----

complexBodmasEval: String -> Int
complexBodmasEval.x = complexEvaluator.(simpWP.(reverse.(stols.x.[].[])).[].[])

--Expression to String 
exptostr.(Val.x)=x
exptostr.(Bracket.x)="("++exptostr.x++")"
exptostr.(SubExp.op.x.y)=exptostr.x++optostr.op++exptostr.y

stols.[].t.f = if t==[] then f else f ++ [t]
stols.(x::xs).t.f = if (ord.x >= 48 && ord.x <= 57) then stols.xs.(t++[x]).f else if (t == []) then stols.xs.[].(f ++ [[x]]) else stols.xs.[].(f ++ [t] ++ [[x]])

convertP.[x].[].[].[].p = x
convertP.[x].[].ep.op.(p::ps) = convertP.(ep ++ [x]).op.[].[].ps
convertP.(x::y::s1).(o::s2).ep.op.(p::ps) = if o == p then convertP.((SubExp.(strtoop.o).x.y)::s1).s2.ep.op.(p::ps) else convertP.(y::s1).s2.(ep ++ [x]).(op ++ [o]).(p::ps)

convert.s1.s2 = convertP.s1.s2.[].[].["^", "*", "%", "/", "+", "-"]

simpWP.[].s1.s2 = convertP.s1.s2.[].[].["^", "*", "%", "/", "+", "-"]
simpWP.(x::xs).s1.s2 = if (ord.(head.x) >= 48 && ord.(head.x) <= 57) then simpWP.xs.((Val.x) :: s1).s2 else if x == "(" then simpWP.xs.((convert.(take.(length.(takeb.s2.0.[])*2).s1).(takeb.s2.0.[])) :: drop.(length.(takeb.s2.0.[])*2).s1).(skip.s2.0) else simpWP.xs.s1.(x::s2)

ctoi : Char -> Int
ctoi.x = (ord.x) - 48
listToNum : [Int] -> Int
listToNum.[x] = x
listToNum.(x::y::xs) = listToNum.(((10*x)+y)::xs)

strtoint.x=(map.ctoi;listToNum).x
 
complexEvaluator.(Val.x)=strtoint.x
complexEvaluator.(Bracket.x)=complexEvaluator.x
complexEvaluator.(SubExp.Plus.x.y)=(+).(complexEvaluator.x).(complexEvaluator.y)
complexEvaluator.(SubExp.Minus.x.y)=(-).(complexEvaluator.x).(complexEvaluator.y)
complexEvaluator.(SubExp.Mul.x.y)=(*).(complexEvaluator.x).(complexEvaluator.y)
complexEvaluator.(SubExp.Div.x.y)=(/).(complexEvaluator.x).(complexEvaluator.y)
complexEvaluator.(SubExp.Exponent.x.y)=(^).(complexEvaluator.x).(complexEvaluator.y)
complexEvaluator.(SubExp.Modulo.x.y)=(mod).(complexEvaluator.x).(complexEvaluator.y)

skip.(")"::xs).0=xs
skip.(")"::xs).n=skip.xs.(n-1)
skip.("("::xs).n=skip.xs.(n+1)
skip.(x::xs).n=skip.xs.n

takeb.(")"::xs).0.l=l
takeb.(")"::xs).n.l=takeb.xs.(n-1).(l++[")"])
takeb.("("::xs).n.l=takeb.xs.(n+1).(l++["("])
takeb.(x::xs).n.l=takeb.xs.n.(l++[x])


