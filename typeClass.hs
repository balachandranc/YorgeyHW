class Meq a where
    eq :: a -> a -> Bool
    neq :: a -> a -> Bool
    eq x y = not $ neq x y
    neq x y = not $ eq x y

data TrafficLight = Red | Yellow | Green

instance Meq TrafficLight where
    eq Red Red = True
    eq Yellow Yellow = True
    eq Green Green = True
    eq _ _ = False

instance Meq Int where
    eq x y = x == y

instance Show TrafficLight where
    show Red = "Red Light"
    show Yellow = "Yellow Light"
    show Green = "Green Light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True 

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf condVar trueVal falseVal =  if yesno condVar
                                    then trueVal
                                    else falseVal