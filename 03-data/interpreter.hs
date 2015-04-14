module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st k v = \key -> if key == k then v else st key

empty :: State
empty = \_ -> 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Var s) = st s
evalE st (Val i) = i
evalE st (Op leftExpr b rightExpr) = case b of
    Plus -> left + right
    Minus -> left - right
    Times -> left * right
    Divide -> left `div` right
    Gt -> toInt $ left > right
    Ge -> toInt $ left >= right
    Lt -> toInt $ left < right
    Le -> toInt $ left <= right
    Eql -> toInt $ left == right
    where left  = evalE st leftExpr
          right = evalE st rightExpr
          toInt bool = if bool then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s exp) = DAssign s exp
desugar (Incr s) = DAssign s $ Op (Var s) Plus (Val 1)
desugar (If expr thenStmt elseStmt) = DIf expr (desugar thenStmt) (desugar elseStmt)
desugar (While expr stmt) = DWhile expr (desugar stmt)
desugar (For startStmt expr incStmt doStmt) = DSequence start (DWhile e d)
    where start = desugar startStmt
          e     = expr
          d     = DSequence (desugar doStmt) (desugar incStmt)
desugar (Sequence a b) = DSequence (desugar a) (desugar b)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s $ evalE st e
evalSimple st (DIf e stmt1 stmt2) = if a then b else c
    where a = evalE st e /= 0
          b = evalSimple st stmt1
          c = evalSimple st stmt2

evalSimple st (DWhile e stmt)
    | bool      = evalSimple newSt (DWhile e stmt)
    | otherwise = st
    where bool = evalE st e /= 0
          newSt = evalSimple st stmt

evalSimple st (DSequence stmt1 stmt2) = evalSimple (evalSimple st stmt1) stmt2

evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st $ desugar stmt

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l


{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
