module Main where

import Prelude

import Control.Parallel (parTraverse_)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.String.Regex (Regex)
import Data.String.Regex (replace) as R
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex) as R
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff)
import Effect.Aff.AVar as Aff
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import HTTP (get, readToEnd)
import Node.Encoding (Encoding(..))
import Node.FS.Stream as FS
import Node.Stream as S
import RSS (parseString)


nonAlphaNumeric :: Regex
nonAlphaNumeric = R.unsafeRegex "[^A-Za-z0-0_-]" global
  
main :: Effect Unit
main = launchAff_ do
  rss <-
    get "https://feeds.feedburner.com/HarmontownPodcast"
    >>= readToEnd UTF8
    >>= parseString


  rss.items # inParallelMax 5 \item -> do
    let filename = R.replace nonAlphaNumeric "_" item.title
    log ("downloading " <> item.title <> " to " <> filename)
    stream <- get item.enclosure.url
    out <- liftEffect $ FS.createWriteStream ("data/" <> filename <> ".mp3")
    pipe stream out
    log ("downloaded " <> item.title)

  log "done!"
    
inParallelMax :: forall a f. (Foldable f) => Int -> (a -> Aff Unit) -> f a -> Aff Unit
inParallelMax max f items = do
  queue <- mkQueue max
  items # parTraverse_ \item -> inQueue queue (f item)
  
pipe :: forall r w. S.Readable r -> S.Writable w -> Aff Unit
pipe src dest = makeAff $ \cb -> do
  _ <- S.pipe src dest
  S.onEnd src (cb $ Right unit)
  pure mempty

log :: String -> Aff Unit
log = liftEffect <<< Console.log

foreign import setTimeoutImpl :: Int -> EffectFnAff Unit

setTimeout :: Int -> Aff Unit
setTimeout = fromEffectFnAff <<< setTimeoutImpl

newtype Queue = Queue (Aff.AVar Int)

mkQueue :: Int -> Aff Queue
mkQueue n = Queue <$> Aff.new n

take :: Queue -> Aff Unit
take (Queue var) = do
  n <- Aff.take var
  if n <= 0 then do
    Aff.put 0 var
    setTimeout 50
    take (Queue var)
  else do
    Aff.put (n - 1) var
    pure unit

replace :: Queue -> Aff Unit
replace (Queue var) = do
  n <- Aff.take var
  Aff.put (n + 1) var

inQueue :: forall a. Queue -> Aff a -> Aff a
inQueue q eff = do
  take q
  result <- eff
  replace q
  pure result
