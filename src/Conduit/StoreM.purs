module Conduit.StoreM where

import Prelude
import Apiary as Apiary
import Conduit.Api.Endpoints as Endpoints
import Conduit.Api.Utils (makeRequest, makeSecureRequest)
import Conduit.Capability.Api (class ArticleApi, class CommentApi, class FavoriteApi, class FollowApi, class ProfileApi, class TagApi, class UserApi)
import Conduit.Capability.Auth (class Auth)
import Conduit.Capability.Routing (class Routing)
import Conduit.Data.Auth (toAuth)
import Conduit.Data.Error (Error(..))
import Conduit.Data.Route (Route)
import Conduit.Data.Env (Env)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (expand, match)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Type.Equality (class TypeEquals, from)
import Wire.React.Atom.Class (modify, read)

newtype StoreM m a
  = StoreM (ReaderT Env m a)

runStoreM :: forall m. Env -> StoreM m ~> m
runStoreM env (StoreM m) = runReaderT m env

derive newtype instance functorStoreM :: Functor m => Functor (StoreM m)

derive newtype instance applyStoreM :: Apply m => Apply (StoreM m)

derive newtype instance applicativeStoreM :: Applicative m => Applicative (StoreM m)

derive newtype instance bindStoreM :: Bind m => Bind (StoreM m)

derive newtype instance monadStoreM :: Monad m => Monad (StoreM m)

derive newtype instance monadEffectStoreM :: MonadEffect m => MonadEffect (StoreM m)

derive newtype instance monadAffStoreM :: MonadAff m => MonadAff (StoreM m)

instance monadAskStoreM :: (TypeEquals e Env, Monad m) => MonadAsk e (StoreM m) where
  ask = StoreM $ asks from

-- | Capabilities
instance authStoreM :: MonadEffect m => Auth (StoreM m) where
  read = ask >>= \{ auth } -> liftEffect $ read auth.signal
  login token profile = ask >>= \{ auth } -> liftEffect $ modify auth.signal $ const $ toAuth token (Just profile)
  logout = ask >>= \{ auth } -> liftEffect $ modify auth.signal $ const Nothing
  updateProfile profile = ask >>= \{ auth } -> liftEffect $ modify auth.signal $ map $ _ { profile = Just profile }

instance routingStoreM :: MonadEffect m => Routing Route (StoreM m) where
  navigate route = ask >>= \{ router } -> liftEffect $ router.navigate route
  redirect route = ask >>= \{ router } -> liftEffect $ router.redirect route

instance userApiStoreM :: MonadAff m => UserApi (StoreM m) where
  loginUser user = do
    res <- makeRequest (Apiary.Route :: Endpoints.LoginUser) Apiary.none Apiary.none { user }
    pure $ res >>= match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors }
  registerUser user = do
    res <- makeRequest (Apiary.Route :: Endpoints.RegisterUser) Apiary.none Apiary.none { user }
    pure $ res >>= (match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })
  updateUser user = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.UpdateUser) Apiary.none Apiary.none { user }
    pure $ res >>= (match { ok: Right <<< _.user, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })

instance articleApiStoreM :: MonadAff m => ArticleApi (StoreM m) where
  listArticles query = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListArticles) Apiary.none query Apiary.none
    pure $ res >>= match { ok: Right }
  listFeed query = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.ListFeed) Apiary.none query Apiary.none
    pure $ res >>= match { ok: Right }
  getArticle slug = do
    res <- makeRequest (Apiary.Route :: Endpoints.GetArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= (match { ok: Right <<< _.article, notFound: Left <<< NotFound })
  submitArticle slug article = do
    res <- case slug of
      Nothing -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.CreateArticle) Apiary.none Apiary.none { article }
      Just slug' -> map expand <$> makeSecureRequest (Apiary.Route :: Endpoints.UpdateArticle) { slug: slug' } Apiary.none { article }
    pure $ res >>= (match { ok: Right <<< _.article, unprocessableEntity: Left <<< UnprocessableEntity <<< _.errors })
  deleteArticle slug = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= (match { ok: const $ Right unit })

instance favoriteApiStoreM :: MonadAff m => FavoriteApi (StoreM m) where
  toggleFavorite { slug, favorited } = do
    res <-
      if favorited then
        makeSecureRequest (Apiary.Route :: Endpoints.UnfavoriteArticle) { slug } Apiary.none Apiary.none
      else
        makeSecureRequest (Apiary.Route :: Endpoints.FavoriteArticle) { slug } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.article }

instance commentApiStoreM :: MonadAff m => CommentApi (StoreM m) where
  listComments slug = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListComments) { slug } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.comments }
  createComment slug comment = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.CreateComment) { slug } Apiary.none { comment }
    pure $ res >>= (match { ok: Right <<< _.comment })
  deleteComment slug id = do
    res <- makeSecureRequest (Apiary.Route :: Endpoints.DeleteComment) { slug, id } Apiary.none Apiary.none
    pure $ res >>= (match { ok: const $ Right unit })

instance profileApiStoreM :: MonadAff m => ProfileApi (StoreM m) where
  getProfile username = do
    res <- makeRequest (Apiary.Route :: Endpoints.GetProfile) { username } Apiary.none Apiary.none
    pure $ res >>= (match { ok: Right <<< _.profile, notFound: Left <<< NotFound })

instance followApiStoreM :: MonadAff m => FollowApi (StoreM m) where
  toggleFollow { username, following } = do
    res <-
      if following then
        makeSecureRequest (Apiary.Route :: Endpoints.UnfollowProfile) { username } Apiary.none Apiary.none
      else
        makeSecureRequest (Apiary.Route :: Endpoints.FollowProfile) { username } Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.profile }

instance tagApiStoreM :: MonadAff m => TagApi (StoreM m) where
  listTags = do
    res <- makeRequest (Apiary.Route :: Endpoints.ListTags) Apiary.none Apiary.none Apiary.none
    pure $ res >>= match { ok: Right <<< _.tags }
