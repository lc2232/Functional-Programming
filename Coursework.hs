
--------------------------------------------
--                                        --
-- CM20256/CM50262 Functional Programming --
--                                        --
--         Coursework 2020/2021           --
--                                        --
--------------------------------------------


------------------------- Auxiliary functions

find :: (Show a,Eq a) => a -> [(a,b)] -> b
find x [] = error ("find: " ++ show x ++ " not found")
find x ((y,z):zs)
  | x == y    = z
  | otherwise = find x zs

-- merges two lists togetehr in order
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

-- recurses through a list and removes any duplicate elements
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
  | elem x xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

-- gets all the odd indexed items from a list 
odds :: [a] -> [a]
odds (x : _ : xs) = x : odds xs

-- gets all the even indexed items from a list
evens :: [a] -> [a]
evens (_ : x : xs) = x : evens xs
------------------------- Lambda-terms

type Var = String

data Term =
    Variable Var
  | Lambda   Var  Term
  | Apply    Term Term
  deriving Eq


instance Show Term where
  show = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x n) = free n `minus` [x]
free (Apply  n m) = free n `merge` free m


------------------------- Types

infixr 5 :->

type Atom = String
data Type = At Atom | Type :-> Type
  deriving Eq

instance Show Type where
  show (At a)       = a
  show (At a :-> s) = a ++ " -> " ++ show s
  show    (t :-> s) = "(" ++ show t ++ ") -> " ++ show s


atoms :: [Atom]
atoms = map (:[]) ['a'..'z'] ++ [ a : show i | i <- [1..], a <- ['a'..'z'] ]

t1 :: Type
t1 = At "a" :-> At "b"

t2 :: Type
t2 = (At "c" :-> At "d") :-> At "e"

t3 :: Type
t3 = At "a" :-> At "c" :-> At "c"


------------------------- Assignment 1

-- (a) "determines if an atom occurs in a type"
occurs :: Atom -> Type -> Bool
occurs a (At x) = a == x -- base case (check if a == x)
occurs a (x :-> xs) -- recursive case. Here we need to handle x itself, and xs as well. So we do:
    | occurs a x = True
    | otherwise = occurs a xs

-- (b) "returns the atoms occurring in a type in an (alphabetically) ordered list"
findAtoms :: Type -> [Atom]
findAtoms (At a) = [a] -- base case single atom
findAtoms (a :-> b) = merge (findAtoms a) (findAtoms b)  -- merges two lists together in order

------------------------- Type substitution

type Sub = (Atom,Type)

s1 :: Sub
s1 = ("a", At "e")

s2 :: Sub
s2 = ("e", At "b" :-> At "c")

s3 :: Sub
s3 = ("c", At "a" :-> At "a")


------------------------- Assignment 2

-- (a) "that applies a substitution to a type"
sub :: Sub -> Type -> Type
sub (a,t) (At x)
    | At a == At x = t
    | otherwise = At x
sub (a,t) (x:->xs)
    | At a == x = t :-> sub (a,t) xs
    | otherwise = sub (a,t) x :-> sub (a,t) xs


-- (b) "applies a list of substitutions to a type, with the head of the list applied last, and the tail applied first"
subs :: [Sub] -> Type -> Type
subs [x] t = sub x t
subs (x:xs) t = subs [x] (subs xs t)


------------------------- Unification

type Upair = (Type,Type)
type State = ([Sub],[Upair])

u1 :: Upair
u1 = (t1,At "c")

u2 :: Upair
u2 = (At "a" :-> At "a",At "a" :-> At "c")

u3 :: Upair
u3 = (t1,t2)

u4 :: Upair
u4 = (t2,t3)

u5 :: Upair
u5 = (t1,t1)

st1 :: State
st1 = ([],[u1,u2])


u6 :: [Upair]
u6 = [(At "a" :-> At "a" :-> At "b" :-> At "b" :-> At "c" :-> At "a" :-> At "c", At "alpha" :-> At "beta" :-> At "gamma" :-> At "beta")]

u7 :: [Upair]
u7 = [( At "b" :-> At "c" ,At "b" :-> At "b")]

st2 :: State
st2 = ([],[u5])


------------------------- Assignment 3

-- (a) "applies a substitution [τ/α] to a list of unification pairs U"
sub_u :: Sub -> [Upair] -> [Upair]
sub_u s xs = map (sub_u_pair s) xs
  where 
    -- auxilary function that performs sub on a single upair
    sub_u_pair :: Sub -> (Type, Type) -> (Type, Type)
    sub_u_pair s (ft,sd) = (sub s ft, sub s sd)


-- takes a upair and turns it into a sub
uToSub :: (b, Type) -> (Atom, b)
uToSub (ft, At sd) = (sd, ft)

-- splits a upair into a list of upairs
splitPairs :: (Type, Type) -> [Upair]
splitPairs (x:->xs,y:->ys) = [(x,y),(xs,ys)]

-- (b) "carries out a single transition of the unification algo- rithm "
step :: State -> State
step (s, (At x, At y):ys)
    | x == y = (s, ys)
step (s, (At x, y):ys) 
    | occurs x y = error "FAIL CASE"
    | otherwise = (s ++ [uToSub (y, At x)], sub_u (uToSub (y, At x)) ys)
step (s, (x, At y):ys) -- opposite pattern as above
    | occurs y x = error "FAIL CASE"
    | otherwise = (s ++ [uToSub (x, At y)], sub_u (uToSub (x, At y)) ys)
step (s, (x, y):ys) = (s, splitPairs (x,y) ++ ys)

-- applies step until state is (S, ∅)
steps :: ([Sub], [Upair]) -> ([Sub], [Upair])
steps (s,[]) = (s,[])
steps st = steps (step st)

-- (c)
unify :: [Upair] -> [Sub]
unify us = reverse (fst (steps ([], us)))


------------------------- Assignment 4
-- (a)
type Context   = [(Var, Type)]
type Judgement = (Context, Term, Type)

data Derivation =
    Axiom Judgement
  | Abstraction Judgement Derivation
  | Application Judgement Derivation Derivation

-- end of (a)
n1 = Apply (Lambda "x" (Variable "x")) (Variable "y")



n2 = Apply(Lambda "x" (Variable "x"))


d1 = Application ([("y",At "a")], n1 , At "a") (
       Abstraction ([("y",At "a")],Lambda "x" (Variable "x"),At "a" :-> At "a") (
         Axiom ([("x",At "a"),("y",At "a")],Variable "x",At "a")
     ) ) (
       Axiom ([("y",At "a")], Variable "y", At "a")
     )

d2 = Application ([("y",At "b")],Apply (Lambda "x" (Apply (Variable "x") (Variable "y"))) (Lambda "z" (Variable "z")),At "a") (
       Abstraction ([("y",At "b")],Lambda "x" (Apply (Variable "x") (Variable "y")),At "c") (
         Application ([("x",At "d"),("y",At "b")],Apply (Variable "x") (Variable "y"),At "e") (
           Axiom ([("x",At "d"),("y",At "b")],Variable "x",At "f")
         ) (
           Axiom ([("x",At "d"),("y",At "b")],Variable "y",At "g")
     ) ) ) (
       Abstraction ([("y",At "b")],Lambda "z" (Variable "z"),At "h") (
         Axiom ([("z",At "i"),("y",At "b")],Variable "z",At "j")
     ) )

-- (b) "extracts the concluding Judgement from a derivation."
conclusion :: Derivation -> Judgement
conclusion (Axiom j) = j
conclusion (Abstraction j _) = j
conclusion (Application j _ _) = j

-- (c) applies a list of substitutions to every type in a context
subs_ctx :: [Sub] -> Context -> Context
subs_ctx _ [] = []
subs_ctx s [(v,t)] = [(v, subs s t)]
subs_ctx s ((v,t):xs) = [(v, subs s t)] ++ subs_ctx s xs

-- (c) applies a list of substitutions to every type in a judgements
subs_jdg :: [Sub] -> Judgement -> Judgement
subs_jdg s (c,tm,ty) = (subs_ctx s c, tm, subs s ty)

-- (c) applies a list of substitutions to every type in a derivation
subs_der :: [Sub] -> Derivation -> Derivation
subs_der s (Axiom j) = Axiom (subs_jdg s j)
subs_der s (Abstraction j dv) = Abstraction (subs_jdg s j) (subs_der s dv)
subs_der s (Application j dv1 dv2) = Application (subs_jdg s j) (subs_der s dv1) (subs_der s dv2)



------------------------- Typesetting derivations


instance Show Derivation where
  show d = unlines (reverse strs)
    where
      (_,_,_,strs) = showD d

      showC :: Context -> String
      showC [] = []
      showC [(x,t)]    = x ++ ": " ++ show t
      showC ((x,t):cx) = x ++ ": " ++ show t  ++ " , " ++ showC cx

      showJ :: Judgement -> String
      showJ ([],n,t) =              "|- " ++ show n ++ " : " ++ show t
      showJ (cx,n,t) = showC cx ++ " |- " ++ show n ++ " : " ++ show t

      showL :: Int -> Int -> Int -> String
      showL l m r = replicate l ' ' ++ replicate m '-' ++ replicate r ' '

      showD :: Derivation -> (Int,Int,Int,[String])
      showD (Axiom j) = (0,k,0,[s,showL 0 k 0]) where s = showJ j; k = length s
      showD (Abstraction j d)   = addrule (showJ j) (showD d)
      showD (Application j d e) = addrule (showJ j) (sidebyside (showD d) (showD e))

      addrule :: String -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      addrule x (l,m,r,xs)
        | k <= m     = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL  l m r  : xs)
        | k <= l+m+r = (ll,k,rr, (replicate ll ' ' ++ x ++ replicate rr ' ') : showL ll k rr : xs)
        | otherwise  = (0,k,0, x : replicate k '-' : [ replicate (-ll) ' ' ++ y ++ replicate (-rr) ' ' | y <- xs])
        where
          k = length x
          i = div (m - k) 2
          ll = l+i
          rr = r+m-k-i

      extend :: Int -> [String] -> [String]
      extend i strs = strs ++ repeat (replicate i ' ')

      sidebyside :: (Int,Int,Int,[String]) -> (Int,Int,Int,[String]) -> (Int,Int,Int,[String])
      sidebyside (l1,m1,r1,d1) (l2,m2,r2,d2)
        | length d1 > length d2 = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ "  " ++ y | (x,y) <- zip d1 (extend (l2+m2+r2) d2)])
        | otherwise             = ( l1 , m1+r1+2+l2+m2 , r2 , [ x ++ "  " ++ y | (x,y) <- zip (extend (l1+m1+r1) d1) d2])


------------------------- Test cases

n3 :: Term
n3 = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Variable "x") (Variable "z"))(Apply (Variable"y") (Variable "z")))))

n4 :: Term
n4 = Lambda "w" ((Variable "w"))

n5 :: Term
n5 = (( Lambda "y" (Lambda "z"(Variable "y"))))

n8 = (Lambda "f" (Lambda "g" (Lambda "x" (Apply (Variable "g") (Apply (Apply (Variable "f") (Variable "x")) (Variable "x")))))) 

n10 :: Term
n10 =Apply (n8) (n5)


n6 :: Term
n6 = Lambda "x" (Lambda "x" (Lambda "y" (Lambda "y" (Apply (Variable "x") (Variable "y")))))   

n7 = Apply (Lambda "x" (Variable "x")) (Lambda "y" (Apply (Variable "y") (Variable "y")))




------------------------- Assignment 5

-- checks a context if the (Var,Type) exists within it, 
-- the context gets updated, if not exists then (Var, Type) added to end of context
update :: (Var, Type) -> Context -> Context
update (v,t) [] = [(v,t)]
update (v1,t1) ((v2,t2) :cs)
  | v1 == v2 = (v1,t1) : cs
  | otherwise = (v2, t2) : update (v1,t1) cs

-- takes a list of variables and turns them into a context with blank types
varToCont0 :: [Var] -> Context
varToCont0 = map (\ x -> (x, At ""))

-- gets free variables of input term and put into context
free0 :: Term -> Context
free0 x = varToCont0 (free x)

-- (a) "creates an impartial derivation from a lambda-term where all types are 'empty'"
derive0 :: Term -> Derivation
-- takes a term, finds its free variables puts them into a context then passes the judgement
derive0 x = aux (free0 x,x,At "")
  where
    aux :: Judgement -> Derivation
    aux (c,Variable x,_) = Axiom (reverse c,Variable x,At "")
    aux (c,Lambda x y,_) = Abstraction (reverse c, Lambda x y, At "") (aux (update (x, At "") c , y, At ""))
    aux (c,Apply x y,_)  = Application (reverse c, Apply x y, At "") (aux (c, x, At "")) (aux (c, y, At ""))


-- assigns atoms to a list of variables and adds them to the context
varToCont1 :: [Atom] -> [Var] -> Context
varToCont1 as vs = aux [] as vs
  where
    aux :: Context -> [Atom] -> [Var] -> Context
    aux _ _ [] = []
    aux _ (a:as) [v] = [(v, At a)]
    aux [] (a:as) (v:vs) = aux [(v,At a)] as vs
    aux cs (a:as) (v:vs) = aux ((v,At a):cs ) as vs

-- gets free variables of input term and put into context
free1 :: [Atom] -> Term -> Context
free1 ats t = varToCont1 ats (removeDuplicates (free t)) 

-- (b) "creates an impartial derivation from a lambda-term where all types are atoms"
derive1 :: Term -> Derivation
derive1 x = aux (evens atoms) (free1 (odds atoms) x, x, At (head atoms))
  where
    aux :: [Atom] -> Judgement -> Derivation
    -- Variable pattern match
    aux l (c,Variable x,_) = Axiom (reverse c,Variable x,At (head l))
    -- Lambda pattern match
    aux l (c,Lambda x y,_) = Abstraction (reverse c, Lambda x y,At (head l)) (aux (evens (drop 1 l)) (update (x, At (head (odds (drop 1 l)))) c ,y,At (l !! 1)))
    -- Apply pattern match
    aux l (c,Apply x y,_) = Application (reverse c, Apply x y,At (head l)) (aux (odds (drop 1 l)) (c ,x,At (l !! 1))) (aux (evens (drop 1 l)) (c ,y,At (l !! 2)))


-- (c) "extracts the type unification pairs from an incom- plete derivation"
upairs :: Derivation -> [Upair]
-- pattern match on axiom
upairs (Axiom (c,Variable x,t)) = [(t, find x c)]
-- various pattrens to matchin on an abstraction 
upairs (Abstraction (c1,Lambda x1 y1,t1) (Axiom (c2,Variable x2,t2))) =  (t1,find x1 c2 :-> t2) : upairs (Axiom (c2,Variable x2,t2))
upairs (Abstraction (c1,Lambda x1 y1,t1) (Abstraction (c2,Lambda x2 y2,t2) m )) = (t1, find x1 c2:-> t2) : upairs (Abstraction (c2,Lambda x2 y2,t2) m )
upairs (Abstraction (c1,Lambda x1 y1,t1) (Application (c2,Apply x2 y2,t2) m n)) = (t1,find x1 c2:-> t2) : upairs (Application (c2,Apply x2 y2,t2) m n)
-- pattern match on application
upairs (Application (c,x,t) m n) = [(getType m, getType n :-> t)] ++ upairs m ++ upairs n
  where
    -- extracts the type from a term
    getType :: Derivation -> Type
    getType (Axiom (_,_,t)) = t
    getType (Abstraction (_,_,t) _) = t
    getType (Application (_,_,t) _ _) = t

-- (d) "takes a term and produces a type derivation for it, if one exists"
derive :: Term -> Derivation
derive t = subs_der (unify (upairs (derive1 t))) (derive1 t)


