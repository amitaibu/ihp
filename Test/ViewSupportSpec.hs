{-|
Module: Test.ViewSupportSpec

Tests for view support functions.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test.RouterSupportSpec where
import qualified Prelude
import ClassyPrelude
import Test.Hspec
import IHP.Test.Mocking hiding (application)
import IHP.Prelude
import IHP.QueryBuilder
import IHP.Environment
import IHP.FrameworkConfig
import IHP.HaskellSupport
import IHP.RouterSupport hiding (get)
import IHP.FrameworkConfig
import IHP.Job.Types
import IHP.Controller.RequestContext hiding (request)
import IHP.ViewPrelude
import IHP.ControllerPrelude hiding (get, request)
import qualified IHP.Server as Server
import Data.Attoparsec.ByteString.Char8 (string, Parser, (<?>), parseOnly, take, endOfInput, choice, takeTill, takeByteString)
import Network.Wai
import Network.Wai.Test
import Network.HTTP.Types
import qualified IHP.ErrorController as ErrorController
import Data.String.Conversions
import Unsafe.Coerce
import IHP.ApplicationContext

data WebApplication = WebApplication deriving (Eq, Show, Data)

data TestController
    = TestAction
    | TestWithParamAction { param :: Text }
  deriving (Eq, Show, Data)

instance Controller TestController where
    action TestAction = do
        renderPlain "TestAction"
    action TestWithParamAction { .. } = do
        let output = [plain|isActiveAction {param}: {isActiveAction $ TestWithParamAction param}|]
        renderPlain $ cs output

instance AutoRoute TestController

instance FrontController WebApplication where
  controllers = [ startPage TestAction, parseRoute @TestController ]

defaultLayout :: Html -> Html
defaultLayout inner =  [hsx|{inner}|]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout

instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]

instance Worker RootApplication where
    workers _ = []

testGet :: ByteString -> Session SResponse
testGet url = request $ setPath defaultRequest { requestMethod = methodGet } url

assertSuccess :: ByteString -> SResponse -> IO ()
assertSuccess body response = do
    response.simpleStatus `shouldBe` status200
    response.simpleBody `shouldBe` (cs body)

assertFailure :: SResponse -> IO ()
assertFailure response = do
    response.simpleStatus `shouldBe` status400

config = do
    option Development
    option (AppPort 8000)

application :: (?applicationContext :: ApplicationContext) => Application
application = Server.application ErrorController.handleNotFound

tests :: Spec
tests = beforeAll (mockContextNoDatabase WebApplication config) do
    describe "isActiveAction" $ do
        it "should return True on the same " $ withContext do
            runSession (testGet "test/TestWithParamAction?param=foo") application >>= assertSuccess "TestAction"