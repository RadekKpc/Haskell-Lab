-- product type example (one constructor)
type X = Int
type Y = Int
data CartInt2DVec = MkCartInt2DVec X Y -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> X
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Y
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue | White | Red

type ActorName =  String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue = "Julette sadas"
leadingActor White = "asd as"
leadingActor Red = "asd a  ss sa "
