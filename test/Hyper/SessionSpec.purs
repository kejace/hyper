module Hyper.SessionSpec where

import Prelude
import Control.Monad.Indexed ((:*>))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Control.Monad.Writer.Trans (WriterT, execWriterT, runWriterT)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Foreign.Object (Object)
import Foreign.Object as Object
import Data.Tuple (Tuple(..), fst)
import Hyper.Conn (Conn)
import Hyper.Cookies (Values, cookies)
import Hyper.Middleware (Middleware, evalMiddleware, runMiddleware)
import Hyper.Node.Session.Cookie (CookieStore(..), mkSecret)
import Hyper.Node.Session.InMemory (newInMemorySessionStore)
import Hyper.Response (HeadersOpen, class Response)
import Hyper.Session (class SessionStore, delete, deleteSession, get, getSession, newSessionID, put, saveSession)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)

type MyAff b state
  = WriterT (TestResponse b state) Aff

saveSession' ::
  forall b state req res store c session.
  Response res (MyAff b state) b =>
  SessionStore store (MyAff b state) session =>
  session ->
  Middleware
    (MyAff b state)
    ( Conn
        req
        (res HeadersOpen)
        { sessions :: { key :: String, store :: store }
        , cookies :: Either String (Object Values)
        | c
        }
    )
    ( Conn
        req
        (res HeadersOpen)
        { sessions :: { key :: String, store :: store }
        , cookies :: Either String (Object Values)
        | c
        }
    )
    Unit
saveSession' = saveSession

run :: forall w m a. Functor m => WriterT w m a -> m a
run = runWriterT >>> map fst

getCookie :: Array (Tuple String String) -> String
getCookie [ Tuple "Set-Cookie" c ] = c
getCookie _ = ""

testStore ::
  forall store session b state.
  SessionStore store (MyAff b state) session =>
  Show session =>
  Eq session =>
  Monoid session =>
  Aff store -> session -> session -> Spec Unit
testStore store session session' = do
  -- | it "retrieves data that was stored" do
  -- |   store' <- store
  -- |   liftAff (session `shouldNotEqual` session')
  -- |   id <- run $ newSessionID store'
  -- |   id' <- run $ put store' id session
  -- |   sessionOut <- run $ get store' id'
  -- |   sessionOut `shouldEqual` Just session
  -- |   id1 <- run $ newSessionID store'
  -- |   id1' <- run $ put store' id1 session'
  -- |   sessionOut' <- run $ get store' id1'
  -- |   sessionOut' `shouldEqual` Just session'
  -- |   sessionOutSecond <- run $ get store' id'
  -- |   sessionOutSecond `shouldEqual` Just session
  -- |   id2 <- run $ newSessionID store'
  -- |   blankSession <- run $ get store' id2
  -- |   blankSession `shouldEqual` Nothing
  -- |   run $ delete store' id'
  -- |   sessionOutDeleted <- run $ get store' id
  -- |   sessionOutDeleted `shouldEqual` Nothing
  it "works with getSession/saveSession/deleteSession" do
    store' <- store
    Tuple sessionOut _ <-
      { request: TestRequest defaultRequest
      , response: TestResponse Nothing [] []
      , components:
        { sessions: { key: "session", store: store' }
        , cookies: unit
        }
      }
        # runMiddleware (cookies :*> getSession)
        >>> run
    sessionOut `shouldEqual` Nothing
    TestResponse _ headers _ <-
      { request: TestRequest defaultRequest
      , response: TestResponse Nothing [] []
      , components:
        { sessions: { key: "session", store: store' }
        , cookies: unit
        }
      }
        # evalMiddleware (cookies :*> saveSession' session)
        >>> execWriterT
    let
      newCookies = getCookie headers
    Tuple sessionOut' _ <-
      { request: TestRequest defaultRequest { headers = Object.singleton "cookie" newCookies }
      , response: TestResponse Nothing [] []
      , components:
        { sessions: { key: "session", store: store' }
        , cookies: unit
        }
      }
        # runMiddleware (cookies :*> getSession)
        >>> run
    sessionOut' `shouldEqual` Just session
    Tuple response (TestResponse _ headers' _) <-
      { request: TestRequest defaultRequest { headers = Object.singleton "cookie" newCookies }
      , response: TestResponse Nothing [] []
      , components:
        { sessions: { key: "session", store: store' }
        , cookies: unit
        }
      }
        # evalMiddleware (cookies :*> getSession :*> deleteSession)
        >>> runWriterT
    let
      newCookies' = getCookie headers'
    Tuple sessionOut'' _ <-
      { request: TestRequest defaultRequest { headers = Object.singleton "cookie" newCookies' }
      , response: TestResponse Nothing [] []
      , components:
        { sessions: { key: "session", store: store' }
        , cookies: unit
        }
      }
        # runMiddleware (cookies :*> getSession)
        >>> run
    sessionOut'' `shouldEqual` Nothing

spec :: Spec Unit
spec = do
  describe "Hyper.Node.Session.InMemory" do
    testStore (liftEffect newInMemorySessionStore) "value1" "value2"
  -- | describe "Hyper.Node.Session.Cookie" do
  -- |   testStore (CookieStore <$> mkSecret) "value1" "value2"
