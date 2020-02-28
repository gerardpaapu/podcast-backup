module RSS where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept, runExceptT)
import Control.Promise (Promise, toAff)
import Data.Either (Either(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign, renderForeignError)
import Prelude ((>>=))
import Simple.JSON as JSON

type Feed =
  { items :: Array { title :: String
                   , pubDate :: String
                   , enclosure :: { url :: String }
                   }
  }

foreign import parseStringImpl :: String -> Effect (Promise Foreign)

parseString_ :: String -> Aff Foreign
parseString_ s = liftEffect (parseStringImpl s) >>= toAff

parseString :: String -> Aff Feed
parseString s = do
  obj <- parseString_ s
  case runExcept $ JSON.readImpl obj of
    Left errs -> do
      let err = head errs
      throwError (error $ renderForeignError err)
    Right v -> pure v
