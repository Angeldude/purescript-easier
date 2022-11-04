module Ch5
  ( test
  )
  where

import Prelude (Unit, show, discard, (+))

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null ("abc" : Nil)
  snoc (1 : 2 : Nil) 3 # show # log
  1 : 2 : 3 : Nil # length # show # log
  log $ show (head Nil :: Maybe Unit) 
  log $ show $ head ("abc" : "123" : Nil)
  log $ show (tail Nil :: Maybe (List Unit))
  log $ show $ tail ("abc" : "123" : Nil)

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

length :: forall a. List a -> Int
length x = length' 0 x where
  length' :: forall b. Int -> List b -> Int
  length' acc Nil = acc
  length' acc (l : ls) = length' (acc+1) ls

snoc :: forall a. List a -> a -> List a
snoc Nil y =  singleton y
snoc (x : xs) y = x : snoc xs y

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x 

const :: forall a b. a -> b -> a
const a _ = a

apply :: forall a b. (a -> b) -> a -> b
apply f x =  f x

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

infixr 0 apply as $
infixl 1 applyFlipped as #