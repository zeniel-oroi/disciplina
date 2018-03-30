
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ExplicitForAll  #-}

module Common (module Common, module Control.Lens, module T) where

import qualified Prelude (show)
import Universum

-- import Control.Arrow (second)
import Control.Lens (to, each)

-- import Data.Bits                                 (xor)
import Data.Default                         as T (Default(def))
import Data.Foldable                        as T (for_)
-- import Data.Function                             (on)
-- import Data.List                                 (sortBy, nubBy)
-- import Data.Monoid                               ((<>))
-- import Data.Ord                                  (comparing)

import System.IO.Unsafe

import qualified Data.Tree.AVL         as AVL
import qualified Disciplina.WorldState as World
import qualified Debug.Trace           as Debug

import Test.Framework                       as T (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 as T (testProperty)
import Test.QuickCheck                      as T ( Arbitrary (..), Gen, Property
                                                 , (===), (==>), elements
                                                 , vectorOf, oneof, suchThat
                                                 , Testable
                                                 , ioProperty, expectFailure
                                                 )
import Test.QuickCheck.Instances            as T ()

-- | Extensional equality combinator.
(.=.) :: (Eq b, Show b, Arbitrary a) => (a -> b) -> (a -> b) -> a -> Property
f .=. g = \a ->
  let fa = f a
      ga = g a

  in  fa === ga

infixr 5 .=.

data Sandbox = Sandbox
    { sWorld        :: World.WorldState
    , sTransaction  :: [World.WithProof World.Transaction]
    , alice         :: World.Entity
    , eve           :: World.Entity
    , bob           :: World.Entity
    , initialAmount :: World.Amount
    }

instance Show Sandbox where
    show (Sandbox world transactions a e b lim) =
        concat
          [ "Sandbox { world = "
          , show world
          , ", transactions = "
          , map (^.World.wpBody) transactions^.to Prelude.show
          , ", alice = "
          , show a
          , ", eve = "
          , show e
          , ", bob = "
          , show b
          , ", lim = "
          , show lim
          , " }"
          ]

instance Arbitrary Sandbox where
    arbitrary = do
        actors @ [alice, eve, bob] <- vectorUniqueOf 3

        let world = fairWorld 10 actors

        transactions <- generateTransactions world alice [eve, bob]

        return $ Sandbox world transactions alice eve bob 10

      where
        generateTransactions world actor rest =
            vectorOf 2 $ do
                changes <- vectorOf 5 $ oneof
                    [ World.TransferTokens <$> elements rest <*> pure 1
                    , World.Publicate      <$> arbitrary
                    ]

                accountCreations <- accountCreation 5 (actor : rest)

                let server = World.Server world

                return $ unsafePerformPureWorldT actor server $ do
                    transaction <- World.plan (changes <> accountCreations)
                    World.connectTransaction transaction

        accountCreation 0     _           = return []
        accountCreation count excludedSet = do
            entity <- noneof excludedSet
            rest   <- accountCreation (count - 1) (entity : excludedSet)
            return (World.CreateAccount entity def : rest)

fairWorld :: World.Amount -> [World.Entity] -> World.WorldState
fairWorld amount actors =
    let
      (world, _) = unsafePerformIO $ do
        (AVL.runOnEmptyCache :: AVL.HashMapStore World.Hash AVL.NullStore World.WorldState -> IO (World.WorldState, AVL.Storage World.Hash)) $ do
            World.evalWorldT def (World.Server World.emptyWorldState) $ do
                World.giveEach actors amount

    in
        world

unsafePerformPureWorldT :: forall side a . World.Entity -> side -> World.WorldT side (AVL.HashMapStore World.Hash AVL.NullStore) a -> a
unsafePerformPureWorldT who side action =
    let
      (a, _) = unsafePerformIO $ do
        AVL.runOnEmptyCache $ do
            World.evalWorldT who side $ do
                action
    in
        a

noneof :: (Arbitrary a, Eq a, Show a) => [a] -> Gen a
noneof set = do
    res <- arbitrary `suchThat` (`notElem` set)
    -- Debug.traceShow (res, "<-/-", set) $
    return res

vectorUniqueOf :: (Arbitrary a, Eq a, Show a) => Int -> Gen [a]
vectorUniqueOf = loop []
  where
    loop acc 0 = return acc
    loop acc n = do
        next <- noneof acc
        loop (next : acc) (n - 1)

instance Arbitrary World.Entity where
  arbitrary = World.Entity <$> (noneof [0])

instance Arbitrary World.Publication where
  arbitrary = World.hash <$> (arbitrary :: Gen Int)

worldTProperty
    :: Testable prop
    => side
    -> World.WorldT side (AVL.HashMapStore World.Hash AVL.NullStore) prop
    -> Property
worldTProperty side what = ioProperty $ do
    (prop, _) <- AVL.runOnEmptyCache $ World.evalWorldT def side what
    return prop
