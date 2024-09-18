module StrictlySorted

------------------------------------------------------------------
-- Attempts to define a strictly sorted list data type (as a set
-- implementation).
------------------------------------------------------------------

--------------------------------------------------------------------
-- StrictlySorted using (Maybe a) as type argument to capture the
-- first element of the list.
--------------------------------------------------------------------

data StrictlySorted : (a : Type) -> Maybe a -> Type where
  Empty : StrictlySorted a Nothing
  Singleton : (x : a) -> StrictlySorted a (Just x)
  Prepend : Ord a => (x : a)
                  -> (s : StrictlySorted a (Just y))
                  -> (prf : (x < y) === True)
                  -> StrictlySorted a (Just x)

-- Test
empty_ssl : StrictlySorted Int Nothing
empty_ssl = Empty

singleton_ssl : StrictlySorted Int (Just 10)
singleton_ssl = Singleton 10

pair_ssl : StrictlySorted Int (Just 5)
pair_ssl = Prepend 5 (Singleton 10) Refl

-- Fails due to 42 not being less than 10
-- ill_ssl : StrictlySorted Int (Just 42)
-- ill_ssl = Prepend 42 (Singleton 10) Refl

------------------------------------------------------------------------
-- Alternative implementation of StrictlySorted not using (Maybe a) in
-- the type declaration.  Not as elegant to define, but easier to use.
-- Note that (Maybe a) is still used in ssh (short for strictly sorted
-- head).  It works because (Just x) < Nothing is evaluated to False.
------------------------------------------------------------------------

-- Pre-declaration to avoid circular dependencies
data StrictlySorted' : Type -> Type
0 ssh : StrictlySorted' a -> Maybe a

data StrictlySorted' : (a : Type) -> Type where
  Empty' : StrictlySorted' a
  Singleton' : (x : a) -> StrictlySorted' a
  Prepend' : Ord a => (x : a)
                   -> (s : StrictlySorted' a)
                   -> (prf : ((Just x) < ssh s) === True)
                   -> StrictlySorted' a

-- Post-implementation of ssh to avoid circular dependencies
ssh Empty' = Nothing
ssh (Singleton' x) = Just x
ssh (Prepend' x _ _) = Just x

-- Test
empty_ssl' : StrictlySorted' Int
empty_ssl' = Empty'

singleton_ssl' : StrictlySorted' Int
singleton_ssl' = Singleton' 10

pair_ssl' : StrictlySorted' Int
pair_ssl' = Prepend' 5 (Singleton' 10) Refl

-- Fails due to 42 not being less than 10
-- ill_ssl' : StrictlySorted Int (Just 42)
-- ill_ssl' = Prepend 42 (Singleton 10) Refl

--------------------------------------
-- Alternatives from Micheal Messer
--------------------------------------

-- data StrictlySorted : Ord a -> List a -> Type where
--     Empty : StrictlySorted o []
--     Singleton : StrictlySorted o [x]
--     Prepend : (v : a) -> StrictlySorted o (x :: xs) -> ((<) @{o} v x = True) -> StrictlySorted o (v :: x :: xs)

-- g : Ord a => (xs : List a) -> {auto prf : StrictlySorted %search xs} -> ()

-- t : ()
-- t = g [1,2,3]

---------------------------------------------
-- Another alternative from Micheal Messer
---------------------------------------------

-- data StrictlySorted : (a : Type) -> Ord a -> Type
-- 0 helper : a -> StrictlySorted a o -> Type

-- public export
-- data StrictlySorted : (a : Type) -> Ord a -> Type where
--     Nil : StrictlySorted a o
--     (::) : (v : a) -> (ls : StrictlySorted a o) -> {auto 0 prf : helper v ls} -> StrictlySorted a o

-- helper v [] = ()
-- helper v (x :: xs) = (<) @{o} v x = True

-- g : Ord a => StrictlySorted a %search -> ()

-- t : ()
-- t = g [1,2,3]
