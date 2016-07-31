#!/usr/bin/env runhaskell
{-
Coder Radio Challenge for Episode #215
https://www.reddit.com/r/CoderRadio/comments/4ukt4k/episode_215_coding_challenge/

Run with Docker Compose: ./start.bash

Run without Docker: ./katy_playlist.hs

Requirements:
  - GHC
  - runhaskell
  - hoauth2

This program will prompt the user to authenticate with Google by copy and pasting the
authentication URI into a web browser. The user also needs post the OAuth2 authentication
code back into this program.

After authenticating with Google, the program will create a private playlist
and then continue to add several videos.

This code is mostly lifted from the hoauth2 example here:
https://github.com/freizl/hoauth2/blob/master/example/Google/test.hs

Modification were made to allow for JSON request body posts.

The Data.Aeson library handles JSON encoding and decoding. JSON is derived from Haskell types.
The OAuth2 functions are handled by the Network.HTTP and Network.Oauth.Oauth2 libaries.

-}

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

import            Network.OAuth.OAuth2

import            System.IO
import            System.Environment
import qualified  Data.ByteString.Char8         as BS
import qualified  Data.ByteString.Lazy.Internal as BL
import qualified  Network.HTTP.Types            as HT
import qualified  Network.HTTP.Types.Header     as HTA
import            Network.HTTP.Conduit
import            Data.Text                     (Text, unpack)
import            Data.Aeson.TH                 (defaultOptions, deriveJSON) 
import qualified  Data.CaseInsensitive          as CI
import            Data.Aeson                    (encode, toJSON, Value, FromJSON) 
import            Control.Monad                 (liftM)

data Token = Token  { issued_to   :: Text
                    , audience    :: Text
                    , user_id     :: Maybe Text
                    , scope       :: Text
                    , expires_in  :: Integer
                    , access_type :: Text
                    } deriving (Show)
$(deriveJSON defaultOptions ''Token)

-- YouTube Types which encode and decode JSON
data YouTubeResourceId = YtResourceId { kind    :: Text
                                      , videoId :: Text
                                      } deriving (Show)
$(deriveJSON defaultOptions ''YouTubeResourceId)

data YouTubeSnippet = YtSnippet { title       :: Maybe Text
                                , playlistId  :: Maybe Text
                                , resourceId  :: Maybe YouTubeResourceId
                                } deriving (Show)
$(deriveJSON defaultOptions ''YouTubeSnippet)
defaultYtSnippet = YtSnippet Nothing Nothing Nothing

-- Only care about the 'id' value from Response
data YouTubePlaylistResponse = YtPlResponse { id :: Text } deriving (Show)
$(deriveJSON defaultOptions ''YouTubePlaylistResponse)

data YouTubeRequest = YtRequest { snippet :: YouTubeSnippet } deriving (Show)
$(deriveJSON defaultOptions ''YouTubeRequest)


main :: IO ()
main = do
  putStrLn "You will be prompted to authenticate with Google so that a fabulous \
           \Katy Perry playlist can be created on your YouTube account. You can \
           \press CTRL-C to stop at any time.\n\n\
           \Do you want to continue? (y\\n)"
  c <- getLine
  if c == "n" then putStrLn "Good-bye" else do 
          
  mgr <- newManager tlsManagerSettings
  BS.putStrLn $ authorizationUrl googleKey `appendQueryParam` googleScopeYouTube
  putStrLn "Visit the url and paste code here: "
  code <- fmap BS.pack getLine
  (Right token) <- fetchAccessToken mgr googleKey code

  validateToken mgr token -- Normally validateToken result should be checked for exception

  -- Create playlist
  putStrLn "Creating playlist ..."
  (Right plRes) <- (ytCreatePlaylist mgr token "Katy Perry" :: IO (OAuth2Result YouTubePlaylistResponse))

  -- Add items to playlist
  putStrLn "Adding items..."
  --mapM (ytCreatePlaylistItem mgr token (Main.id plRes))
  mapM (addItemsWithUserFeedback $ ytCreatePlaylistItem mgr token (Main.id plRes))
    [ ("0KSOMA3QBU0","Dark Horse")
    , ("CevxZvSJLk8","Roar")
    , ("7RMQksXpQSk","This is How We Do")
    , ("98WtmW-lfeE","Teenage Dream")
    , ("KlyXNRrsk4A","Last Friday Night")
    , ("IjRqh9iJ0yM","International Smile")
    , ("XjwZAa2EjKA","Unconditionally")
    ]

  putStrLn "You should now be able to see the new playlist at:"
  putStrLn $ "https://www.youtube.com/playlist?list=" ++ unpack (Main.id plRes)
  putStrLn "Enjoy!"

  where 
    addItemsWithUserFeedback f (videoId, videoName) = do
      putStrLn (" -> " ++ videoName)
      f videoId

-- | oauthCallback = Just "https://developers.google.com/oauthplayground"
googleKey :: OAuth2
googleKey = OAuth2 { oauthClientId = "1009838644286-dkghf2jd5jm4o7mp87rg18qi60vloq69.apps.googleusercontent.com"
                   -- Apparently Google OAuth2 secrets are OK in desktop apps?
                   -- https://developers.google.com/identity/protocols/OAuth2InstalledApp
                   -- "Unlike with web clients, the OAuth 2.0 client secret (if used) is
                   --  assumed not to be confidential."
                   , oauthClientSecret = "onPYgzEHwwQairpO_ndoQKII"
                   , oauthCallback = Just "urn:ietf:wg:oauth:2.0:oob"
                   , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                   , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
                   }

-- | Ask for access to youtube api
googleScopeYouTube :: QueryParams
googleScopeYouTube = [("scope", "https://www.googleapis.com/auth/youtube.force-ssl")]

-- | Token Validation
validateToken :: Manager
              -> AccessToken
              -> IO (OAuth2Result BL.ByteString)
validateToken mgr token =
   authGetBS' mgr token url
   where url = "https://www.googleapis.com/oauth2/v1/tokeninfo"

-- | Playlist requests
ytPlaylists :: Manager
            -> AccessToken
            -> IO (OAuth2Result BL.ByteString)
ytPlaylists mgr token = authGetBS mgr token $ 
  appendQueryParam "https://www.googleapis.com/youtube/v3/playlists" 
    [ ("part","contentDetails")
    , ("mine","true")
    ]

ytCreatePlaylist  :: FromJSON a
                  => Manager
                  -> AccessToken 
                  -> Text                 -- Playlist title
                  -> IO (OAuth2Result a)
ytCreatePlaylist mgr token title =  ytAuthPostJSON mgr token
  (appendQueryParam "https://www.googleapis.com/youtube/v3/playlists" 
    [ ("part","snippet")])                -- URI Params and Full URI
    (toJSON $ YtRequest $ defaultYtSnippet { title = Just title })   -- PostBody as JSON

ytCreatePlaylistItem  :: Manager
                      -> AccessToken
                      -> Text             -- Playlist id
                      -> Text             -- Video id
                      -> IO (OAuth2Result BL.ByteString)
ytCreatePlaylistItem mgr token pl video = ytAuthPostBS mgr token
  (appendQueryParam "https://www.googleapis.com/youtube/v3/playlistItems"
    [ ("part","snippet")])                -- URI Params and Full URI
    (toJSON $ YtRequest $
      defaultYtSnippet { playlistId = Just pl
                       , resourceId = Just (YtResourceId "youtube#video" video)
                       })

-- Create new authPostBS which sets Content-Type correctly for YouTube
-- | Conduct POST request.
ytAuthPostBS :: Manager                           -- ^ HTTP connection manager.
             -> AccessToken
             -> URI                               -- ^ URL
             -> Value
             -> IO (OAuth2Result BL.ByteString)   -- ^ Response as ByteString
ytAuthPostBS manager token url json = do
  req <- parseUrlThrow $ BS.unpack url
  authRequest req upReq manager
  where upBody = (\req' -> req' { requestBody = RequestBodyLBS $ encode json })
        upHeaders = updateRequestHeaders (Just token) . setMethod HT.POST . setContentType "application/json"
        upReq = upHeaders . upBody

-- Same as ytAuthPostBS but returns JSON
ytAuthPostJSON :: FromJSON a
               => Manager                           -- ^ HTTP connection manager.
               -> AccessToken
               -> URI                               -- ^ URL
               -> Value
               -> IO (OAuth2Result a)   -- ^ Response as ByteString
ytAuthPostJSON m t u j = liftM parseResponseJSON $ ytAuthPostBS m t u j

-- | Set the Content-Type header to a new string value.
setContentType :: BS.ByteString -> Request -> Request
setContentType newCT req = req { requestHeaders = updateCT $ requestHeaders req }
  where ct = HTA.hContentType
        updateCT :: HTA.RequestHeaders -> HTA.RequestHeaders
        updateCT ((h,v):xs)       
          | h == ct   = (ct,newCT):xs
          | otherwise = (h,v):(updateCT xs)
        updateCT []   = (ct,newCT):[]
