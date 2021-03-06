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
extend prev symbol val = (\x -> if x == symbol
                                  then val
                                  else prev x )

empty :: State
empty = (\_ -> 0 )

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var varName) = state varName
evalE _ (Val value) = value
evalE state ( Op left Plus right )  = ( evalE state left ) + ( evalE state right )
evalE state ( Op left Minus right ) = ( evalE state left ) - ( evalE state right )
evalE state ( Op left Times right ) = ( evalE state left ) * ( evalE state right )
evalE state ( Op left Divide right ) = ( evalE state left ) `quot` ( evalE state right )
evalE state ( Op left Gt right ) = if ( evalE state left ) > ( evalE state right ) then 1 else 0
evalE state ( Op left Ge right ) = if ( evalE state left ) >= ( evalE state right ) then 1 else 0
evalE state ( Op left Lt right ) = if ( evalE state left ) < ( evalE state right ) then 1 else 0
evalE state ( Op left Le right ) = if ( evalE state left ) <= ( evalE state right ) then 1 else 0
evalE state ( Op left Eql right ) = if ( evalE state left ) == ( evalE state right ) then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)  = DAssign s e
desugar (Incr s)      = DAssign s ( Op ( Var s ) Plus ( Val 1 ) )
desugar (If c t e)    = DIf c ( desugar t ) ( desugar e )
desugar (While c s)   = DWhile c ( desugar s )
desugar (Sequence s1 s2) = DSequence ( desugar s1 ) ( desugar s2 )
desugar Skip          = DSkip
desugar (For init cond update body) = DSequence ( desugar init ) ( DWhile cond ( DSequence ( desugar body ) ( desugar update ) ) )

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign varName expression) = extend state varName ( evalE state expression )
evalSimple state (DIf cond thenS elseS) = if (evalE state cond) == 1
                                            then evalSimple state thenS
                                            else evalSimple state elseS
evalSimple state loop@(DWhile cond stmt) = if ( evalE state cond ) == 1
                                            then evalSimple state ( DSequence stmt loop )
                                            else state
evalSimple state (DSequence s1 s2) = evalSimple ( evalSimple state s1 ) s2
evalSimple state DSkip             = state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

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
