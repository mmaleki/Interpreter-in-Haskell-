import Data.List
import System.IO


maxInt2 = maxBound ::Int

j=3::Int

addFloat :: Float -> Float -> Float
addFloat x y=x+y

foo::Int -> Bool

foo n
   | n `mod` 2 ==0 =True
   | otherwise=False        

foo2 :: Double -> Double -> Bool

foo2 x y
   | mx >10=False
   |mx<9=True
   where mx=max x y

stringEq::[Char]->[Char]->Bool
stringEq [] []=True
stringEq (x:xs) (y:ys) = x==y && stringEq xs ys
stringEq _ _ =False

foo3::Int -> String

foo3 n= case n of
    2 -> "Prime"
    3 -> "Prime"
    _ -> "Composed"

data Shape=Circle Float Float Float 
          |Rectangle Float Float Float Float
        deriving Show

area::Shape -> Float
area x = case x of
    Circle _ _ r -> pi*r**2
    Rectangle x1 y1 x2 y2 -> (abs (x1-x2))* (abs (y1-y2))

data Dual a = Dual a a
       deriving Show

dualAdd :: (Num a) => Dual a -> Dual a -> Dual a
dualAdd (Dual x dx) (Dual y dy)=Dual (x+y) (dx+dy)

dualMul :: (Num a) => Dual a -> Dual a -> Dual a
dualMul (Dual x dx) (Dual y dy)=Dual (x*y) (y*dx+x*dy)

primal::(Num a) => Dual a -> a
primal (Dual x dx)=x


tangent::(Num a) => Dual a -> a
tangent (Dual x dx)=dx

lift::(Float -> Float) -> (Dual Float -> Dual Float)
lift f (Dual x dx)= case f of
    sin -> (Dual (sin x) (dx * (cos x)))
--    |cos = (Dual (sin x) (dx * (cos x)))
--    cos -> (Dual (cos x) (dx * (sin x)))



testFunction:: (Floating a) => (a -> a) -> a
testFunction sin = 5
testFunction cos =4.0
testFunction _ = 0
