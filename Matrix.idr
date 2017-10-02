import Data.Vect

||| Matrix data type: Nested vects.
||| Matrix: n rows by m columns.
Matrix : Nat -> Nat -> Type
Matrix n m = Vect n (Vect m Nat)

Vector : Nat -> Type
Vector m = Matrix m 1

||| Gets a relevant item from a vector.
getItem : Fin a -> Vect a type -> type
getItem (FZ)   (row :: rows) = row
getItem (FS k) (_ :: rows)   = getItem k rows

||| Gets the relevant row of the matrix. NOTE: Starts counting at 0. 
getRow : Fin a -> Matrix a m -> Matrix 1 m
getRow fin m1 = (getItem fin m1 :: Nil)

||| Gets the relevant column of the matrix.
getColumn : Fin a -> Matrix n a -> Vector n
getColumn col m1 = map ?rhs_getColumnFunctionToMap m1

||| Gets the items in a Vector in a vect. 
getVectorItems : Vector m -> Vect m Nat
getVectorItems = map head

scaleVector : Vector m -> Nat -> Vector m
scaleVector v1 scalar = map (scalarMult scalar) (getVectorItems v1) where
    scalarMult : Nat -> Nat -> Vect 1 Nat
    scalarMult scalar head = (scalar * head :: Nil)

||| Linear combination: Any n x m matrix.
linearCombination : Matrix n m -> Vector m -> Matrix n 1
linearCombination m1 m2 = ?rhs_linearCombination

sampleMatrix : Matrix 3 4
sampleMatrix = (  (1 :: 0 :: 0 :: 1 :: Nil)
               :: (0 :: 1 :: 0 :: 2 :: Nil)
               :: (0 :: 0 :: 1 :: 3 :: Nil)
               :: Nil)

sampleVector : Vector 4
sampleVector = (  (1 :: Nil)
               :: (2 :: Nil)
               :: (3 :: Nil)
               :: (4 :: Nil)
               :: Nil)