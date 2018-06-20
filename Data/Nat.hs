{-# LANGUAGE
  UndecidableInstances, ScopedTypeVariables, DataKinds,
  FlexibleInstances, GADTs, TypeFamilies, TemplateHaskell,
  InstanceSigs, TypeOperators, PolyKinds, StandaloneDeriving,
  FlexibleContexts, AllowAmbiguousTypes, CPP, OverloadedStrings,
  EmptyCase #-}

module Data.Nat (
    Nat(..)
  , NatPlus
  , NatMul
  , NatMinus
  , NatAbs
  , NatSignum
  , natPlus
  , natMul
  , natMinus
  , natAbs
  , natSignum
  , someNatVal
  , SNat
  , Data.Singletons.Prelude.Sing(SS, SZ)
  , Data.Singletons.Prelude.PNum
  , Data.Singletons.Prelude.SNum
  , SSym0(..)
  , SSym1
  , ZSym0
  , Lit
  , LitSym0(..)
  , LitSym1
  , SLit
  , sLit) where

import Data.Singletons.TH
import Data.Singletons.Prelude
import qualified GHC.TypeNats as TN
import Numeric.Natural (Natural)

$(singletons [d|
  data Nat = Z | S Nat deriving (Eq, Show, Ord)

  natPlus :: Nat -> Nat -> Nat
  natPlus Z     b = b
  natPlus (S a) b = S (natPlus a b)

  natMul :: Nat -> Nat -> Nat
  natMul Z     _ = Z
  natMul (S a) b = natPlus b (natMul a b)

  natMinus :: Nat -> Nat -> Nat
  natMinus Z     _     = Z
  natMinus (S a) (S b) = natMinus a b
  natMinus a     Z     = a

  natAbs :: Nat -> Nat
  natAbs n = n

  natSignum :: Nat -> Nat
  natSignum Z     = Z
  natSignum (S _) = S Z

  instance Num Nat where
    (+) = natPlus
    (-) = natMinus
    (*) = natMul
    abs = natAbs
    signum = natSignum
    fromInteger n
      = if n == 0
           then Z
           else S (fromInteger (n - 1))
  |])

#if !(MIN_VERSION_singletons(2,4,0))
deriving instance Show (SNat n)
#endif

instance Eq (SNat n) where
  (==) _ _ = True

instance Ord (SNat n) where
  compare _ _ = EQ

{-| Converts a runtime 'Natural' to an existentially wrapped 'Nat'. -}
someNatVal :: Natural -> SomeSing Nat
someNatVal n = case TN.someNatVal n of
  TN.SomeNat (_ :: Proxy n) -> SomeSing (sFromInteger (sing :: Sing n))

{-| Provides a shorthand for 'Nat'-s using "GHC.TypeNats", for example:

>>> :kind! Lit 3
Lit 3 :: Nat
= 'S ('S ('S 'Z))
-}

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n TN.- 1))
$(genDefunSymbols [''Lit])

type SLit n = Sing (Lit n)

{-| Shorthand for 'SNat' literals using `TypeApplications`.

>>> :set -XTypeApplications
>>> sLit @5
SS (SS (SS (SS (SS SZ))))

-}

sLit :: forall (n :: TN.Nat). SingI (Lit n) => Sing (Lit n)
sLit = sing
