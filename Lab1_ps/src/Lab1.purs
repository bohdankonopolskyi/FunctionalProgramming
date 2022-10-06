module Lab1
  ( length
  , null
  , singleton
  , snoc
  , test
  )
  where

import Data.Array.Partial (tail)
import Data.List (List(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, show, ($), (+))


singleton :: forall a. a -> List a
singleton a = (Cons a Nil)

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton(x)
snoc (Cons head tail) x = Cons head $ snoc tail x


length :: forall a. Array a -> Int
length [] = 0
length arr = 1 + length
  (unsafePartial tail arr)
test::Effect Unit
test = do 
  log $ "Singleton"
  log $ show $ singleton "item"
  log $ "Null empty"
  log $ show $ null (Nil)
  log $ "Mull nonEmpty"
  log $ show $ null ("item" : Nil)
  log $ "Snoc"
  log $ show $ snoc ("item" :Nil) "item2"
  log $ "Length"
  log $ show $ length ("item": "item2": "item3": Nil)

