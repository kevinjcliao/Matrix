import Data.Vect


||| Matrix with n rows and m columns
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Nat)

sampleMatrix : Matrix 3 4
sampleMatrix = (  (1 :: 0 :: 0 :: 1 :: Nil)
               :: (0 :: 1 :: 0 :: 2 :: Nil)
               :: (0 :: 0 :: 1 :: 3 :: Nil)
               :: Nil)
