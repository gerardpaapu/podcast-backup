module HTTP where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (makeAff, Aff)
import Effect.Exception (error)
import Effect.Ref as Ref
import Foreign.Object as O
import Node.Encoding (Encoding)
import Node.HTTP.Client as H
import Node.Stream as S
  
get :: forall r. String -> Aff (S.Readable r)
get = makeAff <<< getImpl
 where
   getImpl url cb = do
     go url cb
     pure mempty

   go url cb = do
         req <- H.requestFromURI url \response -> do
           case H.statusCode response of
             200 -> cb (Right $ H.responseAsStream response)
             301 -> followRedirect response cb
             302 -> followRedirect response cb
             _ -> do
                let e = error $ show (H.statusCode response) <> " " <> H.statusMessage response
                cb (Left e)
         let s = H.requestAsStream req
         S.end s (pure unit)
   followRedirect response cb = do
        let headers = H.responseHeaders response
        case O.lookup "location" headers of
            Just u  -> go u cb
            Nothing -> cb (Left $ error "header missing: Location")

readToEnd :: forall r. Encoding -> S.Readable r -> Aff String
readToEnd enc stream = makeAff go
  where
  go cb = do
    result <- Ref.new ""
    S.onDataString stream enc (\s -> Ref.modify_ (_ <> s) result)
    S.onError stream (\e -> cb (Left e))
    S.onEnd stream (do r <- Ref.read result
                       cb (Right r))
    pure mempty
