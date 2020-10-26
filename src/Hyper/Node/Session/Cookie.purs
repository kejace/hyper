module Hyper.Node.Session.Cookie where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), joinWith, split)
import Hyper.Session (SessionID(..), class SessionStore)
import Node.Crypto.Cipher as Cipher
import Node.Crypto.Decipher as Decipher
import Node.Crypto.Hash as Hash
import Node.Crypto.Hmac as Hmac
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)

foreign import randString :: Effect String

type Keys
  = { hmacKey :: String, cipherKey :: String }

mkRandomSecretKeys :: Effect Keys
mkRandomSecretKeys = do
  hmacKey <- randString
  cipherKey <- randString
  pure { hmacKey, cipherKey }

newtype CookieStore :: forall k. k -> Type
newtype CookieStore session = CookieStore Keys

derive instance newtypeCookieStore :: Newtype (CookieStore session) _

encrypt :: Keys -> String -> Effect String
encrypt { cipherKey, hmacKey } text = do
  encrypted <- Cipher.hex Cipher.AES256 cipherKey text
  hmac <- Hmac.hex Hash.SHA512 hmacKey encrypted
  pure $ joinWith "," [ hmac, encrypted ]

decrypt :: Keys -> String -> Effect (Maybe String)
decrypt { cipherKey, hmacKey } text =
  case split (Pattern ",") text of
      [ hmac, encrypted ] ->
        let
          calcHmac = Hmac.hex Hash.SHA512 hmacKey encrypted

          decryptWhen hmac'
            | hmac == hmac' = Just <$> Decipher.fromHex Cipher.AES256 cipherKey encrypted
          decryptWhen _ = pure Nothing
        in
          calcHmac >>= decryptWhen
      _ -> pure Nothing

instance sessionStoreCookieStore ::
  ( ReadForeign session
  , WriteForeign session
  , Monad m
  , MonadEffect m
  ) =>
  SessionStore (CookieStore session) m session where
  newSessionID _ = pure $ SessionID "new-id"
  get store id = liftEffect do
    text <- decrypt (unwrap store) $ unwrap id
    pure $ text >>= readJSON >>> hush
  put store _ session = liftEffect $ void $ SessionID <$> encrypt (unwrap store) json
    where
    json = writeJSON session
  delete store _ = pure unit
