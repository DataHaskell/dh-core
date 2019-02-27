{-# language FlexibleInstances, FlexibleContexts, TypeFamilies #-}
{-# language ConstrainedClassMethods #-}
{-# language CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Numeric.BLAS.Class
-- Copyright   :  (c) Marco Zocca 2017-2018
-- License     :  see the file LICENSE
--
-- Maintainer  :  ocramz fripost org
-- Stability   :  experimental
-- Portability :  portable
--
-- Typeclasses for linear algebra and related concepts
--
-----------------------------------------------------------------------------
module Core.Numeric.BLAS.Class where

import Data.Complex
import Foreign.C.Types (CFloat, CDouble)


-- * Matrix and vector elements (optionally Complex)
--
-- Uses 'FlexibleContexts' and 'TypeFamilies' 
class (Eq e , Fractional e, Floating e, Num (EltMag e), Ord (EltMag e)) => Elt e where
  type EltMag e :: *
  -- | Complex conjugate, or identity function if its input is real-valued
  conj :: e -> e
  conj = id
  -- | Magnitude
  mag :: e -> EltMag e

instance Elt Double where {type EltMag Double = Double ; mag = id}
instance Elt Float where {type EltMag Float = Float; mag = id}
instance (RealFloat e) => Elt (Complex e) where
  type EltMag (Complex e) = e
  conj = conjugate
  mag = magnitude



infixl 6 ^+^, ^-^

-- * Additive group
class AdditiveGroup v where
  -- | The zero element: identity for '(^+^)'
  zeroV :: v
  -- | Add vectors
  (^+^) :: v -> v -> v
  -- | Additive inverse
  negateV :: v -> v
  -- | Group subtraction
  (^-^) :: v -> v -> v
  (^-^) x y = x ^+^ negateV y


infixr 7 .*

-- * Vector space @v@.
class (AdditiveGroup v, Num (Scalar v)) => VectorSpace v where
  type Scalar v :: *
  -- | Scale a vector
  (.*) :: Scalar v -> v -> v

-- | Adds inner (dot) products.
class VectorSpace v => InnerSpace v where
  -- | Inner/dot product
  (<.>) :: v -> v -> Scalar v

-- | Inner product
dot :: InnerSpace v => v -> v -> Scalar v
dot = (<.>)


-- | Scale a vector by the reciprocal of a number (e.g. for normalization)
(./) :: (VectorSpace v, s ~ Scalar v, Fractional s) => v -> s -> v
v ./ s = recip s .* v

-- | Vector multiplied by scalar
(*.) :: (VectorSpace v, s ~ Scalar v) => v -> s -> v
(*.) = flip (.*)  



-- | Convex combination of two vectors (NB: 0 <= `a` <= 1). 
cvx :: VectorSpace v => Scalar v -> v -> v -> v
cvx a u v = a .* u ^+^ ((1-a) .* v)




-- ** Hilbert-space distance function

-- |`hilbertDistSq x y = || x - y ||^2` computes the squared L2 distance between two vectors
hilbertDistSq :: InnerSpace v => v -> v -> Scalar v
hilbertDistSq x y = t <.> t where
  t = x ^-^ y


-- * Normed vector spaces
--
-- Uses 'ConstrainedClassMethods'
class (InnerSpace v, Num (RealScalar v), Eq (RealScalar v), Epsilon (Magnitude v), Show (Magnitude v), Ord (Magnitude v)) => Normed v where
  type Magnitude v :: *
  type RealScalar v :: *
  -- | L1 norm
  norm1 :: v -> Magnitude v
  -- | Euclidean (L2) norm squared
  norm2Sq :: v -> Magnitude v
  -- | Lp norm (p > 0)
  normP :: RealScalar v -> v -> Magnitude v
  -- | Normalize w.r.t. Lp norm
  normalize :: RealScalar v -> v -> v
  -- | Normalize w.r.t. L2 norm
  normalize2 :: v -> v
  -- | Normalize w.r.t. norm2' instead of norm2
  normalize2' :: Floating (Scalar v) => v -> v 
  normalize2' x = x ./ norm2' x
  -- | Euclidean (L2) norm
  norm2 :: Floating (Magnitude v) => v -> Magnitude v 
  norm2 x = sqrt (norm2Sq x)
  -- | Euclidean (L2) norm; returns a Complex (norm :+ 0) for Complex-valued vectors
  norm2' :: Floating (Scalar v) => v -> Scalar v 
  norm2' x = sqrt $ x <.> x
  -- | Lp norm (p > 0)
  norm :: Floating (Magnitude v) => RealScalar v -> v -> Magnitude v 
  norm p v
    | p == 1 = norm1 v
    | p == 2 = norm2 v
    | otherwise = normP p v



-- | Infinity-norm (Real)
normInftyR :: (Foldable t, Ord a) => t a -> a
normInftyR x = maximum x

-- | Infinity-norm (Complex)
normInftyC :: (Foldable t, RealFloat a, Functor t) => t (Complex a) -> a
normInftyC x = maximum (magnitude <$> x)


-- | Lp inner product (p > 0)
dotLp :: (Set t, Foldable t, Floating a) => a -> t a -> t a ->  a
dotLp p v1 v2 = sum u**(1/p) where
  f a b = (a*b)**p
  u = liftI2 f v1 v2


-- | Reciprocal
reciprocal :: (Functor f, Fractional b) => f b -> f b
reciprocal = fmap recip


-- |Scale
scale :: (Num b, Functor f) => b -> f b -> f b
scale n = fmap (* n)



-- * Matrix ring

-- | A matrix ring is any collection of matrices over some ring R that form a ring under matrix addition and matrix multiplication

class (AdditiveGroup m, Epsilon (MatrixNorm m)) => MatrixRing m where
  type MatrixNorm m :: *
  -- | Matrix-matrix product
  (##) :: m -> m -> m
  -- | Matrix times matrix transpose (A B^T)
  (##^) :: m -> m -> m
  -- | Matrix transpose times matrix (A^T B)
  (#^#) :: m -> m -> m
  a #^# b = transpose a ## b
  -- | Matrix transpose (Hermitian conjugate in the Complex case)
  transpose :: m -> m
  -- | Frobenius norm
  normFrobenius :: m -> MatrixNorm m



-- * Linear vector space

class (VectorSpace v {-, MatrixRing (MatrixType v)-}) => LinearVectorSpace v where
  type MatrixType v :: *
  -- | Matrix-vector action
  (#>) :: MatrixType v -> v -> v
  -- | Dual matrix-vector action
  (<#) :: v -> MatrixType v -> v
  





-- * Typeclasses for internal use (not to be exposed to the user)

-- ** FiniteDim : finite-dimensional objects
class FiniteDim f where
  type FDSize f
  -- | Dimension (i.e. Int for SpVector, (Int, Int) for SpMatrix)
  dim :: f -> FDSize f


-- ** HasData : accessing inner data (do not export)
class HasData f where
  type HDData f 
  -- | Number of nonzeros
  nnz :: f -> Int
  dat :: f -> HDData f


-- ** Sparse : sparse datastructures
class (FiniteDim f, HasData f) => Sparse f where
  -- | Sparsity (fraction of nonzero elements)
  spy :: Fractional b => f -> b


-- ** Set : types that behave as sets
--
-- Example : sparse vector addition operates on the union of the nonzero indices, whereas the sparse vector inner product uses 
class Functor f => Set f where
  -- | Union binary lift : apply function on _union_ of two "sets"
  liftU2 :: (a -> a -> a) -> f a -> f a -> f a
  -- | Intersection binary lift : apply function on _intersection_ of two "sets"
  liftI2 :: (a -> a -> b) -> f a -> f a -> f b






-- * instances

-- | Instances for builtin types
--
-- Uses 'CPP'
#define ScalarType(t) \
instance AdditiveGroup (t) where {zeroV = 0; (^+^) = (+); negateV = negate};\
instance VectorSpace (t) where {type Scalar (t) = t; (.*) = (*) };

-- ScalarType(Int)
-- ScalarType(Integer)
ScalarType(Float)
ScalarType(Double)
ScalarType(Complex Float)
ScalarType(Complex Double)

#undef ScalarType

instance InnerSpace Float  where {(<.>) = (*)}
instance InnerSpace Double where {(<.>) = (*)}
instance InnerSpace (Complex Float)  where {x <.> y = x * conjugate y}
instance InnerSpace (Complex Double) where {x <.> y = x * conjugate y}

#define SimpleNormedInstance(t) \
instance Normed (t) where {type Magnitude (t) = t; type RealScalar (t) = t;\
 norm1 = abs; norm2Sq = (**2); normP _ = abs; normalize _ = signum;\
 normalize2 = signum; normalize2' = signum; norm2 = abs; norm2' = abs; norm _ = abs};

SimpleNormedInstance(Float)
SimpleNormedInstance(Double)

#undef SimpleNormedInstance

#define ComplexNormedInstance(t) \
instance Normed (Complex (t)) where {\
 type Magnitude  (Complex (t)) = t;\
 type RealScalar (Complex (t)) = t;\
 norm1   (r :+ i) = abs r + abs i;\
 norm2Sq (r :+ i) = r*r + i*i;\
 normP p (r :+ i) = (r**p + i**p)**(1/p);\
 normalize p c = toC (1 / normP p c) * c;\
 normalize2  c = (1 / norm2' c) * c;\
 norm2  = magnitude;\
 norm2' = toC . magnitude;};

ComplexNormedInstance(Float)
ComplexNormedInstance(Double)

#undef ComplexNormedInstance


-- * Utilities

-- | Lift a real number onto the complex plane
toC :: Num a => a -> Complex a
toC r = r :+ 0







-- | Provides a test to see if a quantity is near zero.
--
-- Appears originally in ekmett/linear
--
-- TODO[ocramz] Uses 'FlexibleInstances', so might be a good idea to move it in a separate module
--
-- >>> nearZero (1e-11 :: Double)
-- False
--
-- >>> nearZero (1e-17 :: Double)
-- True
--
-- >>> nearZero (1e-5 :: Float)
-- False
--
-- >>> nearZero (1e-7 :: Float)
-- True
class (Floating a, Num a) => Epsilon a where
  -- | Determine if a quantity is near zero.
  nearZero :: a -> Bool

-- | @'abs' a '<=' 1e-6@
instance Epsilon Float where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon Double where
  nearZero a = abs a <= 1e-12

-- | @'abs' a '<=' 1e-6@
instance Epsilon CFloat where
  nearZero a = abs a <= 1e-6

-- | @'abs' a '<=' 1e-12@
instance Epsilon CDouble where
  nearZero a = abs a <= 1e-12


-- | @'magnitude' a '<=' 1e-6@
instance Epsilon (Complex Float) where
  nearZero a = magnitude a <= 1e-6

-- | @'magnitude' a '<=' 1e-12@
instance Epsilon (Complex Double) where
  nearZero a = magnitude a <= 1e-12

-- | @'magnitude' a '<=' 1e-6@
instance Epsilon (Complex CFloat) where
  nearZero a = magnitude a <= 1e-6

-- | @'magnitude' a '<=' 1e-12@
instance Epsilon (Complex CDouble) where
  nearZero a = magnitude a <= 1e-12



-- * Rounding operations


-- | Is this quantity close to 1 ?
nearOne :: Epsilon a => a -> Bool
nearOne x = nearZero (1 - x)

-- | Is this quantity distinguishable from 0 ?
isNz :: Epsilon a => a -> Bool
isNz x = not (nearZero x)

withDefault :: (t -> Bool) -> t -> t -> t
withDefault q d x | q x = d
                  | otherwise = x

roundZero, roundOne, roundZeroOne :: Epsilon a => a -> a
roundZero = withDefault nearZero (fromIntegral (0 :: Int))
roundOne = withDefault nearOne (fromIntegral (1 :: Int))

with2Defaults :: (t -> Bool) -> (t -> Bool) -> t -> t -> t -> t
with2Defaults q1 q2 d1 d2 x | q1 x = d1
                            | q2 x = d2
                            | otherwise = x

-- | Round to respectively 0 or 1
roundZeroOne = with2Defaults nearZero nearOne (fromIntegral (0 :: Int)) (fromIntegral (1 :: Int))
