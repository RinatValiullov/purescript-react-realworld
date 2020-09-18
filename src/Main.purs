module Main (main, handler) where

import Prelude
import Conduit.Component.Auth as Auth
import Conduit.Component.Routing as Routing
import Conduit.Config as Config
import Conduit.Data.Route (Route(..), routeCodec)
import Conduit.Root as Root
import Control.Monad.Reader (runReaderT)
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn3, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import React.Basic (element)
import React.Basic.DOM (hydrate, render)
import React.Basic.DOM as R
import React.Basic.DOM.Server (renderToString)
import Routing.Duplex (parse)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Wire.React.Atom.Sync as Sync
import Wire.Signal as Signal

main :: Effect Unit
main = do
  container <- getElementById "conduit" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Conduit container element not found."
    Just c -> do
      auth /\ authManager <- Auth.mkAuthManager
      routing /\ routingManager <- Routing.mkRoutingManager routeCodec Error
      root <- runReaderT Root.mkRoot { auth, routing }
      case Config.nodeEnv of
        "production" -> hydrate (authManager (routingManager (root unit))) c
        _ -> render (authManager (routingManager (root unit))) c

handler ::
  forall r.
  EffectFn3
    { path :: String | r }
    Foreign
    (EffectFn2 (Nullable Foreign) { body :: String, statusCode :: Int } Unit)
    Unit
handler =
  mkEffectFn3 \event context callback -> do
    body <- do
      auth <- Sync.create { load: pure Nothing, save: \_ -> pure unit }
      routing <- Signal.create $ fromMaybe Error $ hush $ parse routeCodec event.path
      root <-
        runReaderT
          Root.mkRoot
          { auth:
              { signal: auth
              }
          , routing:
              { signal: routing.signal
              , navigate: \_ -> pure unit
              , redirect: \_ -> pure unit
              }
          }
      pure (renderToString (document $ root unit))
    runEffectFn2 callback null
      { statusCode: 200
      , body
      }
  where
  document content =
    R.html
      { children:
          [ R.head
              { children:
                  [ R.meta { charSet: "utf-8" }
                  , R.meta { name: "viewport", content: "width=device-width, initial-scale=1" }
                  , R.title { children: [ R.text "Conduit" ] }
                  , link
                      { href: "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
                      , rel: "stylesheet"
                      , type: "text/css"
                      , media: "print"
                      , onLoad: "this.media='all'; this.onload=null;"
                      }
                  , link
                      { href: "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
                      , rel: "stylesheet"
                      , type: "text/css"
                      , media: "print"
                      , onLoad: "this.media='all'; this.onload=null;"
                      }
                  , R.link
                      { href: "//demo.productionready.io/main.css"
                      , rel: "stylesheet"
                      , type: "text/css"
                      , media: "all"
                      }
                  ]
              }
          , R.body
              { children:
                  [ R.div { id: "conduit", children: [ content ] }
                  , R.script { src: "/index.js" }
                  ]
              }
          ]
      }

  link = element (unsafePerformEffect (R.unsafeCreateDOMComponent "link"))
