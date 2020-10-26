module Hyper.SessionSpec where

import Debug.Trace
import Prelude
import Test.Spec.ShouldEqualOrSatisfy

import Control.Monad.Indexed ((:*>))
import Control.Monad.Writer.Trans (WriterT, execWriterT, runWriterT)
import Data.Either (Either(..))
import Data.Lens as Lens
import Data.Lens.Lens.Tuple as Lens
import Data.Lens.Record as Lens
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Newtype (unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.NonEmpty as NonEmpty
import Data.Predicate (Predicate(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex
import Data.String.Regex.Unsafe as Regex
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import Hyper.Conn (Conn)
import Hyper.Cookies (Values, cookies, setCookie)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware, evalMiddleware, runMiddleware)
import Hyper.Node.Session.Cookie (CookieStore(..), mkRandomSecretKeys)
import Hyper.Node.Session.InMemory (InMemorySessionStore(..), newInMemorySessionStore)
import Hyper.Response (class Response, HeadersOpen)
import Hyper.Session (class SessionStore, delete, deleteSession, get, getSession, newSessionID, put, saveSession)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest)
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual, shouldNotEqual)
import Type.Prelude (SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

type MyAff :: forall k. Type -> k -> Type -> Type
type MyAff bodyChunk stateProxy
  = WriterT (TestResponse bodyChunk stateProxy) Aff

runWriterReturnOutput :: forall w m a. Functor m => WriterT w m a -> m a
runWriterReturnOutput = runWriterT >>> map fst

getSetCookie :: Array (Tuple String String) -> String
getSetCookie [ Tuple "Set-Cookie" setCookie ] = setCookie
getSetCookie _ = ""

testStore ::
  forall store session bodyChunk stateProxy.
  SessionStore store (MyAff Void Void) session =>
  --
  Show session =>
  Eq session =>
  --
  -- | Show bodyChunk =>
  -- | Eq bodyChunk =>
  --
  Monoid session =>
  { mkStore :: Aff store
  , printStore :: store -> Aff (Array (Tuple String String))
  , session :: session
  , resultSession :: session
  } ->
  Spec Unit
testStore { mkStore, printStore, session, resultSession } = do
  it "retrieves data that was stored" do
    store <- mkStore
    liftAff (session `shouldNotEqual` resultSession)
    id <- runWriterReturnOutput $ newSessionID store
    id' <- runWriterReturnOutput $ put store id session
    sessionOut <- runWriterReturnOutput $ get store id'
    sessionOut `shouldEqual` Just session
    id1 <- runWriterReturnOutput $ newSessionID store
    id1' <- runWriterReturnOutput $ put store id1 resultSession
    sessionOut' <- runWriterReturnOutput $ get store id1'
    sessionOut' `shouldEqual` Just resultSession
    sessionOutSecond <- runWriterReturnOutput $ get store id'
    sessionOutSecond `shouldEqual` Just session
    id2 <- runWriterReturnOutput $ newSessionID store
    blankSession <- runWriterReturnOutput $ get store id2
    blankSession `shouldEqual` Nothing
    runWriterReturnOutput $ delete store id'
    sessionOutDeleted <- runWriterReturnOutput $ get store id
    sessionOutDeleted `shouldEqual` Nothing

  -- | it "works with getSession/saveSession/deleteSession" do
  -- |   store <- mkStore

  -- |   (result1 ::
  -- |     ( (Maybe session)
  -- |       /\
  -- |       { components :: { cookies :: Either String (Object (NonEmpty Array String))
  -- |                       , sessions :: { key :: String
  -- |                                     , store :: Array (Tuple String String)
  -- |                                     }
  -- |                       }
  -- |       , request :: TestRequest
  -- |       , response :: TestResponse Void Void
  -- |       }
  -- |     )
  -- |     /\
  -- |     (TestResponse Void Void)
  -- |   ) <-
  -- |     ( runWriterT $ runMiddleware (cookies :*> getSession)
  -- |       { components:
  -- |         { sessions: { key: "session", store }
  -- |         , cookies: unit
  -- |         }
  -- |       , request: TestRequest defaultRequest
  -- |       , response: TestResponse Nothing [] []
  -- |       }
  -- |     ) >>=
  -- |       Lens.over
  -- |       (Lens.traverseOf (Lens._1 <<< Lens._2 <<< Lens.prop (SProxy :: SProxy "components") <<< Lens.prop (SProxy :: SProxy "sessions") <<< Lens.prop (SProxy :: SProxy "store")))
  -- |       printStore

  -- |   result1 `shouldEqual`
  -- |     Tuple
  -- |       (Tuple
  -- |         Nothing
  -- |         { components:
  -- |           { cookies: Right $ Object.empty
  -- |           , sessions:
  -- |             { key: "session"
  -- |             , store: []
  -- |             }
  -- |           }
  -- |         , request: TestRequest defaultRequest
  -- |         , response: TestResponse Nothing [] []
  -- |         }
  -- |       )
  -- |       ( TestResponse Nothing [] []
  -- |       )

  -- |   (result2 ::
  -- |     ( Unit
  -- |       /\
  -- |       { components :: { cookies :: Either String (Object (NonEmpty Array String))
  -- |                       , sessions :: { key :: String
  -- |                                     , store :: Array (Tuple String String)
  -- |                                     }
  -- |                       }
  -- |       , request :: TestRequest
  -- |       , response :: TestResponse Void HeadersOpen
  -- |       }
  -- |     )
  -- |     /\
  -- |     (TestResponse Void Void)
  -- |   ) <-
  -- |     ( runWriterT $ runMiddleware (cookies :*> saveSession session)
  -- |       { components:
  -- |         { sessions: { key: "session", store }
  -- |         , cookies: unit
  -- |         }
  -- |       , request: TestRequest defaultRequest
  -- |       , response: TestResponse Nothing [] []
  -- |       }
  -- |     )
  -- |     >>=
  -- |       Lens.over
  -- |       (Lens.traverseOf (Lens._1 <<< Lens._2 <<< Lens.prop (SProxy :: SProxy "components") <<< Lens.prop (SProxy :: SProxy "sessions") <<< Lens.prop (SProxy :: SProxy "store")))
  -- |       printStore

  -- |   result2 `shouldEqualOrSatisfy`
  -- |     Tuple
  -- |       (Tuple
  -- |         unit
  -- |         { components:
  -- |           { cookies: (Right (Object.empty) :: Either String (Object (NonEmpty Array String)))
  -- |           , sessions:
  -- |             { key: "session"
  -- |             , store: [(Tuple "randomSessionId" "value1")]
  -- |             }
  -- |           }
  -- |         , request: TestRequest defaultRequest
  -- |         , response: TestResponse Nothing ([] :: Array Header) ([] :: Array Void)
  -- |         }
  -- |       )
  -- |       (Predicate $ \(x :: TestResponse Void Void) ->
  -- |         case x of
  -- |              TestResponse Nothing [(Tuple "Set-Cookie" setCookieValue)] ([] :: Array Void) -> Regex.test (Regex.unsafeRegex """^session=\d+\.\d+;HttpOnly;SameSite=Lax$""" Regex.noFlags) setCookieValue
  -- |              _ -> false
  -- |       )

  -- |   (result3 ::
  -- |     ( Maybe session
  -- |       /\
  -- |       { components :: { cookies :: Either String (Object (NonEmpty Array String))
  -- |                       , sessions :: { key :: String
  -- |                                     , store :: Array (Tuple String String)
  -- |                                     }
  -- |                       }
  -- |       , request :: TestRequest
  -- |       , response :: TestResponse Void HeadersOpen
  -- |       }
  -- |     )
  -- |     /\
  -- |     (TestResponse Void Void)
  -- |   ) <-
  -- |     ( runWriterT $ runMiddleware (cookies :*> getSession)
  -- |       { components:
  -- |         { sessions: { key: "session", store }
  -- |         , cookies: unit
  -- |         }
  -- |       , request: TestRequest defaultRequest { headers = Object.singleton "cookie" """session=1000000000000.100""" }
  -- |       , response: TestResponse Nothing [] []
  -- |       }
  -- |     )
  -- |       >>=
  -- |         Lens.over
  -- |         (Lens.traverseOf (Lens._1 <<< Lens._2 <<< Lens.prop (SProxy :: SProxy "components") <<< Lens.prop (SProxy :: SProxy "sessions") <<< Lens.prop (SProxy :: SProxy "store")))
  -- |         printStore

  -- |   result3 `shouldEqualOrSatisfy`
  -- |     Tuple
  -- |       (Tuple
  -- |         (Nothing :: Maybe session)
  -- |         { components:
  -- |           { cookies: (Right (Object.singleton "session" (NonEmpty.singleton "1000000000000.100")) :: Either String (Object (NonEmpty Array String)))
  -- |           , sessions:
  -- |             { key: "session"
  -- |             , store: [(Tuple "randomSessionId" "value1")]
  -- |             }
  -- |           }
  -- |         , request: TestRequest defaultRequest { headers = Object.singleton "cookie" """session=1000000000000.100""" }
  -- |         , response: TestResponse Nothing ([] :: Array Header) ([] :: Array Void)
  -- |         }
  -- |       )
  -- |       (TestResponse Nothing ([] :: Array Header) ([] :: Array Void))

  -- |   -- ?
  -- |   -- | sessionOut' `shouldEqual` Just session

  -- |   -- | Tuple response (TestResponse _ headers' _) <-

  -- |   (result4 ::
  -- |     ( Unit
  -- |       /\
  -- |       { components :: { cookies :: Either String (Object (NonEmpty Array String))
  -- |                       , sessions :: { key :: String
  -- |                                     , store :: Array (Tuple String String)
  -- |                                     }
  -- |                       }
  -- |       , request :: TestRequest
  -- |       , response :: TestResponse Void HeadersOpen
  -- |       }
  -- |     )
  -- |     /\
  -- |     (TestResponse Void Void)
  -- |   ) <-
  -- |     ( runWriterT $ runMiddleware (cookies :*> deleteSession)
  -- |       { components:
  -- |         { sessions: { key: "session", store }
  -- |         , cookies: unit
  -- |         }
  -- |       , request: TestRequest defaultRequest { headers = Object.singleton "cookie" """session=1000000000000.100""" }
  -- |       , response: TestResponse Nothing [] []
  -- |       }
  -- |     )
  -- |       >>=
  -- |         Lens.over
  -- |         (Lens.traverseOf (Lens._1 <<< Lens._2 <<< Lens.prop (SProxy :: SProxy "components") <<< Lens.prop (SProxy :: SProxy "sessions") <<< Lens.prop (SProxy :: SProxy "store")))
  -- |         printStore

  -- |   result4 `shouldEqualOrSatisfy`
  -- |     Tuple
  -- |       (Tuple
  -- |         unit
  -- |         { components:
  -- |           { cookies: (Right (Object.singleton "session" (NonEmpty.singleton "1000000000000.100")) :: Either String (Object (NonEmpty Array String)))
  -- |           , sessions:
  -- |             { key: "session"
  -- |             , store: [(Tuple "randomSessionId" "value1")]
  -- |             }
  -- |           }
  -- |         , request: TestRequest defaultRequest { headers = Object.singleton "cookie" """session=1000000000000.100""" }
  -- |         , response: TestResponse Nothing ([] :: Array Header) ([] :: Array Void)
  -- |         }
  -- |       )
  -- |       (TestResponse Nothing [(Tuple "Set-Cookie" "session=;Max-Age=0")] ([] :: Array Void))

spec :: Spec Unit
spec = do
  describe "Hyper.Node.Session.InMemory" do
    testStore
      { mkStore: liftEffect newInMemorySessionStore
      , printStore: \(InMemorySessionStore store) -> liftEffect do
         store' <- Ref.read store
         pure $ map (Lens.set Lens._1 "randomSessionId") $ Map.toUnfoldable store'
      , session: "value1"
      , resultSession: "value2"
      }
  describe "Hyper.Node.Session.Cookie" do
     testStore
       { mkStore: CookieStore <$> mkRandomSecretKeys
       , printStore: \(CookieStore store) -> pure []
       , session: "value1"
       , resultSession: "value2"
       }
