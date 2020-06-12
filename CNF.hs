
type Id = String 

data Form = Lit Id
          | Neg  Form
          | And Form Form 
          | Or  Form Form 
          | Imply Form Form
          | Equiv Form Form
          deriving (Eq, Show)

--instance Show Form where
--show (Literal v)  = [v]
--show (Neg p)      = "~" ++ show p
--show (And p q)    = "(" ++ show p ++ " & " ++ show q ++ ")"
--show (Or p q)     = "(" ++ show p ++ " | " ++ show q ++ ")"
--show (Imply p q ) = "(" ++ show p ++ " -> " ++ show q ++ ")"
--show (Equivalent p q ) = "(" ++ show p ++ " <-> " ++ show q ++ ")"

infixl 6 <==>
infixl 7 ==>
infixl 8 \/
infixl 9 /\


(/\), (\/), (==>),(<==>):: Form -> Form -> Form
(/\) = And
(\/) = Or
(==>) = Imply
(<==>) = Equiv


type NNF = Form
type CNF = Form

toNNF:: Form -> NNF
toNNF (Neg (Neg p)) = toNNF p
toNNF (Neg (And p q)) = (Or (Neg (toNNF p)) (Neg(toNNF q)))
toNNF (Neg (Or  p q)) = (And (Neg(toNNF p)) (Neg(toNNF q)))
toNNF p = p

toCNF::Form -> CNF
toCNF (Or ( And p q )r) = (And (Or (toCNF (toNNF p)) (toCNF (toNNF r)))
                           (Or (toCNF (toNNF q)) (toCNF (toNNF r))))
toCNF (Or p (And q r)) = (And (Or (toCNF (toNNF p)) (toCNF (toNNF q)))
							   (Or (toCNF (toNNF p)) (toCNF (toNNF r))))
toCNF p = p

step :: Form -> Form
step input = if input == output
    then input
    else step output
    where output = toCNF (toNNF input)

-- some exmples which I tested
-- input: toCNF ((\/) ((/\) (Lit "p") (Lit "q")) (Lit "r"))
-- output: And (Or (Lit "r") (Lit "p")) (Or (Lit "r") (Lit "q"))

-- input: toCNF ((\/) ((/\) (Neg (Neg (Lit "p"))) (Lit "q")) (Lit "r"))
-- output: And (Or (Lit "p") (Lit "r")) (Or (Lit "q") (Lit "r")) 




