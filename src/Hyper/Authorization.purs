module Hyper.Authorization where

import Control.IxMonad (ibind)
import Control.Monad (class Monad)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Unit (unit, Unit)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn, modifyConn)
import Hyper.Response (class ResponseWritable, respond, headers, writeStatus, class ResponseWriter, ResponseEnded, StatusLineOpen)
import Hyper.Status (statusForbidden)

withAuthorization :: forall a b req res c.
                     b
                  -> Conn req res { authorization :: a | c }
                  -> Conn req res { authorization :: b | c }
withAuthorization a conn =
  conn { components = (conn.components { authorization = a }) }


authorized
  :: forall a m req res b c
   . ( Monad m
     , ResponseWritable b m String
     , ResponseWriter res m b
     )
  => (Conn req (res StatusLineOpen) { authorization :: Unit | c } -> m (Maybe a))
  -> Middleware
     m
     (Conn req (res StatusLineOpen) { authorization :: a | c })
     (Conn req (res ResponseEnded) { authorization :: a | c })
     Unit
  -> Middleware
     m
     (Conn req (res StatusLineOpen) { authorization :: Unit | c })
     (Conn req (res ResponseEnded) { authorization :: Unit | c })
     Unit
authorized authorizer mw = do
  conn ← getConn
  auth ← lift' (authorizer conn)
  case auth of
    Just a -> do
      modifyConn (withAuthorization a)
      mw
      modifyConn (withAuthorization unit)
    Nothing -> do
      writeStatus statusForbidden
      headers []
      respond "You are not authorized."
  where bind = ibind
