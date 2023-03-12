module Network.OAuth.Util where

import Keycloak (keycloakClientId, keycloakClientSecret, keycloakHost, keycloakRealm)
import Network.OAuth (freshToken, getTokenByClientCredentials)
import Network.OAuth.Type (AccessTokenResponse, AccessTokenResponseSuccess(..))
import Control.Monad.Except.Trans (throwError)
import Data.Either (either)
import Data.Maybe (Maybe(Just), maybe)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)

-- Throws on Aff error channel
ensureAuthed ::
  Maybe (Tuple String String) -- access_token refresh_token
  -> Aff _ (Tuple String String) -- tokensGetter
  -> Aff _ (Tuple String String)
ensureAuthed tokens tokensGetter =
  maybe
    tokensGetter
    (\tokens' ->
      (fresh tokens' <|> tokensGetter)
    )
    tokens
  where
    fresh :: Tuple String String -> Aff _ (Tuple String String)
    fresh tokens =
      freshToken keycloakHost keycloakRealm
        { client_id: keycloakClientId
        , client_secret: Just keycloakClientSecret
        }
        tokens
