module Conduit.Root where

import Prelude
import Conduit.Component.App as App
import Conduit.Component.Footer as Footer
import Conduit.Component.Header as Header
import Conduit.Components.Toast as Toast
import Conduit.Control.Routing (Completed, Pending, Routing, continue, redirect)
import Conduit.Data.Route (Route(..))
import Conduit.Env (Env)
import Conduit.Env.User (UserSignal)
import Conduit.Hook.Routing (useRoute)
import Conduit.Hook.User (useUser)
import Conduit.Page.Editor (mkEditorPage)
import Conduit.Page.Home (mkHomePage)
import Conduit.Page.Login (mkLoginPage)
import Conduit.Page.Register (mkRegisterPage)
import Conduit.Page.Settings (mkSettingsPage)
import Control.Monad.Indexed.Qualified as Ix
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.JSS as JSS
import React.Basic.DOM as R
import React.Basic.Hooks as React
import Wire.React.Class (read) as Wire

mkRoot :: App.Component Env Unit
mkRoot = do
  homePage <- mkHomePage
  loginPage <- mkLoginPage
  registerPage <- mkRegisterPage
  settingsPage <- mkSettingsPage
  editor <- mkEditorPage
  App.component' "Root" \env props -> React.do
    user <- useUser env
    route <- useRoute env
    pure
      $ React.fragment
          [ Toast.toastManager
          , Header.header user route
          , case route of
              Home -> do
                homePage unit
              Login -> do
                loginPage unit
              Register -> do
                registerPage unit
              Settings -> do
                settingsPage unit
              CreateArticle -> do
                editor { slug: Nothing }
              UpdateArticle slug -> do
                editor { slug: Just slug }
              ViewArticle slug -> do
                React.empty
              Error -> do
                R.text "Error"
              _ -> do
                React.empty
          , Footer.footer
          ]

onNavigate :: UserSignal -> Route -> Routing Pending Completed Unit
onNavigate userSignal route = Ix.do
  auth <- (liftEffect :: _ -> _ Pending Pending _) $ fst <$> Wire.read userSignal
  case route, auth of
    Login, Just _ -> do
      redirect Home
    Register, Just _ -> do
      redirect Home
    Settings, Nothing -> do
      redirect Home
    CreateArticle, Nothing -> do
      redirect Home
    _, _ -> do
      continue

attachGlobalComponentStyles :: Effect Unit
attachGlobalComponentStyles = do
  jssInstance <- JSS.createInstance JSS.preset
  traverse_ (JSS.globalAttachStyleSheet <=< JSS.createStyleSheet jssInstance)
    [ Toast.styles
    ]