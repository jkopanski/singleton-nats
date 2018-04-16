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
import Unsafe.Coerce
import qualified GHC.TypeLits as Lit

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
  |])

#if !(MIN_VERSION_singletons(2,4,0))
deriving instance Show (SNat n)
#endif

instance Eq (SNat n) where
  (==) _ _ = True

instance Ord (SNat n) where
  compare _ _ = EQ

instance Num Nat where
  (+) = natPlus
  (-) = natMinus
  (*) = natMul
  abs = natAbs
  signum = natSignum
  fromInteger 0 = Z
  fromInteger n = S (fromInteger (n - 1))

#if MIN_VERSION_singletons(2,3,0)
instance PNum Nat where
#else
instance PNum ('Proxy :: Proxy Nat) where
#endif
#if MIN_VERSION_singletons(2,4,0)
  type a + b = NatPlus a b
  type a - b = NatMinus a b
  type a * b = NatMul a b
#else
  type a :+ b = NatPlus a b
  type a :- b = NatMinus a b
  type a :* b = NatMul a b
#endif
  type Abs a = NatAbs a
  type Signum a = NatSignum a
  type FromInteger (a :: Lit.Nat) = Lit a

instance SNum Nat where
#if MIN_VERSION_singletons(2,4,0)
  (%+) = sNatPlus
  (%*) = sNatMul
  (%-) = sNatMinus
#else
  (%:+) = sNatPlus
  (%:*) = sNatMul
  (%:-) = sNatMinus
#endif
  sAbs    = sNatAbs
  sSignum = sNatSignum
  sFromInteger n = case n
#if MIN_VERSION_singletons(2,4,0)
                          %==
#else
                          %:==
#endif
                               (sing :: Sing 0) of
    STrue  -> unsafeCoerce SZ
    SFalse -> unsafeCoerce (SS (sFromInteger (n
#if MIN_VERSION_singletons(2,4,0)
                                                %-
#else
                                                %:-
#endif
                                                    (sing :: Sing 1))))

{-| Converts a runtime 'Integer' to an existentially wrapped 'Nat'. Returns 'Nothing' if
the argument is negative -}
someNatVal :: Integer -> Maybe (SomeSing Nat)
someNatVal n = case Lit.someNatVal n of
  Just (Lit.SomeNat (_ :: Proxy n)) -> Just (SomeSing (sFromInteger (sing :: Sing n)))
  Nothing -> Nothing

{-| Provides a shorthand for 'Nat'-s using "GHC.TypeLits", for example:

>>> :kind! Lit 3
Lit 3 :: Nat
= 'S ('S ('S 'Z))
-}

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n Lit.- 1))
$(genDefunSymbols [''Lit])

type SLit n = Sing (Lit n)

{-| Shorthand for 'SNat' literals using `TypeApplications`.

>>> :set -XTypeApplications
>>> sLit @5
SS (SS (SS (SS (SS SZ))))

-}

sLit :: forall (n :: Lit.Nat). SingI (Lit n) => Sing (Lit n)
sLit = sing
