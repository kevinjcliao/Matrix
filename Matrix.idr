import Data.Vect


||| Matrix data type: Nested vects.
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Nat)

||| Gets the relevant row of the matrix. NOTE: Starts counting at 1.
||| This is a library designed for mathematicians.
getRow : Fin a -> Matrix a m -> Matrix 1 m
getRow fin m1 = (search fin m1 :: Nil) where
  search : Fin a -> Matrix a m -> Vect m Nat
  search (FS FZ) (row :: rows) = row
  search (FS k)  (_ :: rows)   = search k rows

||| Linear combination: Any n x m matrix.
linearCombination : Matrix n m -> Matrix n 1 -> Matrix n 1
linearCombination m1 m2 = ?rhs_linearCombination

sampleMatrix : Matrix 3 4
sampleMatrix = (  (1 :: 0 :: 0 :: 1 :: Nil)
               :: (0 :: 1 :: 0 :: 2 :: Nil)
               :: (0 :: 0 :: 1 :: 3 :: Nil)
               :: Nil)
