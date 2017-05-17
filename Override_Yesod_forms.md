### Override Yesod forms

Yesod scaffolded site comes with bundled auth plugin. I usualy use [auth-email](https://hackage.haskell.org/package/yesod-auth-1.4.17/docs/Yesod-Auth-Email.html) package that allows users to register via email. It comes with the rather ugly form that you probably want to override.
If you take a look at source of auth-email package on Hackage (see I am throwing rhymes now) you will see that there are two default handlers exported that we are interested in
```haskell
-- * Default handlers
    , defaultEmailLoginHandler
    , defaultRegisterHandler
```
In order to override login and registration forms you can set these values to your custom handlers like this
```haskell
-- Foundation.hs
instance YesodAuthEmail App where
    type AuthEmailId App = UserId
    registerHandler = myRegisterHandler
    emailLoginHandler = myEmailLoginHandler
    afterPasswordRoute _ = HomeR
    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False Nothing Nothing Haskeller
```
where you will obviously provide `myRegisterHandler` and `myEmailLoginHandler`. Here is how the User entity looks like:
```haskell
-- Model.hs
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email Text
    password Text Maybe -- Password may not be set yet
    verkey Text Maybe -- Used for resetting passwords
    verified Bool
    UniqueUser email
    name Text Maybe
    lastname Text Maybe
    role Role
    deriving Typeable
|]
```
and I should mention that the Role field is just like enum that must be defined in separate file from models:

```haskell
-- Models/Role.hs
{-# LANGUAGE TemplateHaskell #-}
module Models.Role where

import           Database.Persist.TH
import           Prelude

data Role = Admin | Author | Haskeller deriving (Show, Read, Eq)

derivePersistField "Role"
```
here is the registration handler:

```haskell
--Foundation.hs
-- REGISTRATION FORM
-- data types for the forms
data UserForm = UserForm { _userFormEmail :: Text }
data UserLoginForm = UserLoginForm { _loginEmail :: Text, _loginPassword :: Text }

myRegisterHandler :: HandlerT Auth (HandlerT App IO) Html
myRegisterHandler = do
    (widget, enctype) <- lift $ generateFormPost registrationForm
    toParentRoute <- getRouteToParent
    lift $ defaultLayout $ do
        setTitleI Msgs.RegisterLong
        [whamlet|
              <div .col-md-4 .col-md-offset-4>
                <p>_{Msgs.EnterEmail}
                <form method="post" action="@{toParentRoute registerR}" enctype=#{enctype}>
                        ^{widget}
                        <div .voffset4>
                          <button .btn .btn-success .btn-sm .pull-right>_{Msgs.Register}
        |]
    where
        registrationForm extra = do
            let emailSettings = FieldSettings {
                fsLabel = SomeMessage Msgs.Email,
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "true"),("class","form-control")]
            }

            (emailRes, emailView) <- mreq emailField emailSettings Nothing

            let userRes = UserForm <$> emailRes
            let widget = do
                [whamlet|
                    #{extra}
                    ^{fvLabel emailView}
                    ^{fvInput emailView}
                |]

            return (userRes, widget) 

```

And here is the login form handler

```haskell
--Foundation.hs
myEmailLoginHandler :: (Route Auth -> Route App) -> WidgetT App IO ()
myEmailLoginHandler toParent = do
        (widget, enctype) <- liftWidgetT $ generateFormPost loginForm

        [whamlet|
              <div .col-md-4 .col-md-offset-4>
                <form method="post" action="@{toParent loginR}", enctype=#{enctype}>
                    <div id="emailLoginForm">
                        ^{widget}
                        <div .voffset4>
                            <button type=submit .btn .btn-success .btn-sm>Login
                            &nbsp;
                            <a href="@{toParent registerR}" .btn .btn-default .btn-sm .pull-right>
                                _{Msgs.Register}
        |]
  where
    loginForm extra = do

        emailMsg <- renderMessage' Msgs.Email
        (emailRes, emailView) <- mreq emailField (emailSettings emailMsg) Nothing

        passwordMsg <- renderMessage' Msgs.Password
        (passwordRes, passwordView) <- mreq passwordField (passwordSettings passwordMsg) Nothing

        let userRes = UserLoginForm Control.Applicative.<$> emailRes
                                    Control.Applicative.<*> passwordRes
        let widget = do
            [whamlet|
                #{extra}
                <div>
                    ^{fvInput emailView}
                <div>
                    ^{fvInput passwordView}
            |]

        return (userRes, widget)
    emailSettings emailMsg =
        FieldSettings {
            fsLabel = SomeMessage Msgs.Email,
            fsTooltip = Nothing,
            fsId = Just "email",
            fsName = Just "email",
            fsAttrs = [("autofocus", ""), ("placeholder", emailMsg), ("class","form-control")]
        }

    passwordSettings passwordMsg =
         FieldSettings {
            fsLabel = SomeMessage Msgs.Password,
            fsTooltip = Nothing,
            fsId = Just "password",
            fsName = Just "password",
            fsAttrs = [("placeholder", passwordMsg), ("class","form-control")]
        }

    renderMessage' msg = do
        langs <- languages
        master <- getYesod
        return $ renderAuthMessage master langs msg
```

You will need to change the routes offcourse since thay will probably not match with yours and provide imports for messages and auth plugin itself.
```haskell
-- Foundation.hs
import Yesod.Auth.Email
import qualified Yesod.Auth.Message       as Msgs
```
Now we are missing send email functionality as well as fetching the verify key and user password, saving new user password etc. and guess what ? Here it is :

```haskell
    sendVerifyEmail email _ verurl = do
        liftIO $ putStrLn $ "Copy/ Paste this URL in your browser:" DM.<> verurl
        -- Send email.
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textP, htmlP]]
            }
      where
        textP = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                [stext|
                    Please confirm your email address by clicking on the link below.
                    #{verurl}
                    Thank you
                |]
            , partHeaders = []
            }
        htmlP = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml
                [shamlet|
                    <p>Please confirm your email address by clicking on the link below.
                    <p>
                        <a href=#{verurl}>#{verurl}
                    <p>Thank you
                |]
            , partHeaders = []
            }

    getVerifyKey = runDB . fmap (join . fmap userVerkey) . get

    setVerifyKey uid key = runDB $ update uid [UserVerkey =. Just key]

    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid

    getPassword = runDB . fmap (join . fmap userPassword) . get

    setPassword uid pass = runDB $ update uid [UserPassword =. Just pass]

    getEmailCreds email = runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> return $ Just EmailCreds
                { emailCredsId = uid
                , emailCredsAuthId = Just uid
                , emailCredsStatus = isJust $ userPassword u
                , emailCredsVerkey = userVerkey u
                , emailCredsEmail = email
                }

    getEmail = runDB . fmap (fmap userEmail) . get
```

You can play with this example since the code is really self explanatory and if there is something you should take away from this that is:

####  Look at the library code and the exporting functions in order to see what you can override.






