### Yesod - simple CRUD

One thing. Beginners don't know how to read or use library code so things need to be chewed up and explained in plain language for them so they can go further and progress more.  Haskell community is really great and you can get help but it would be better if you could just find tutorials for simple stuff.

Here goes example how to override the forms and do some CRUD actions in Yesod with Persistent.

Oh sorry, [Persistent](https://hackage.haskell.org/package/persistent) is a library that Yesod uses to intteract with DB  and has some really neat features. It lets you forget about underlaying database so you can switch databases easily, It is mapping database results to Haskell types and takes care of migrations for you.

#### How to do simple CRUD ?
If you looked at some Yesod tutorials you will know that routing is organized around Handlers. So every route has a corresponding Handler in which the action happens. I will not cover that , there is an excellent tutorial by Maximilian Tagher on how to use Handlers and create, list and view posts

## [HERE](https://www.youtube.com/watch?v=SadfV-qbVg8). <------

 This guy should do more of those.

I will give you an example that is actually from this website's source code that is basically the same what Maximillian did but I will add edit action which he did not cover. This website was initialized using persistent template, I am guessing you already saw quick start [page](https://www.yesodweb.com/page/quickstart). If you take a peek at config/models you will see some predefined database models that are defined using special syntax. These correspond to database tables and serve to map the database data to the type level. We can define  Tutorial DSL like this :

```
-- config/models
Tutorial
   title Text
   content Markdown 
   deriving Show
```
You can see I am using Markdown type for content because I prefer to edit this page content in markdown since it is fast and easy to edit. You can also pick that part in the linked video tutorial that I added before. So far so good, now you have access to Tutorial type which you can use in your form, something like this:

```
-- this code lives in some Handler

  tutorialForm :: AForm Handler Tutorial
  tutorialForm =
  Tutorial <$> areq textField (bfs ("Title" :: Text)) Nothing
           <*> areq markdownField (bfs ("Content" :: Text)) Nothing
```

This basically means that form fields, once populated and submitted will be used to create new Tutorial data type which we will persist to database with the help of a handler. 
This :
```
bfs('Title' :: Text)
```

will render bootstrap 3 input since it looks nicer that default ones that come with Yesod. I order to use it you need to import Bootstrap3 lib
```
import Yesod.Form.Bootstrap3
```
One thing that confused me when dealing with the form inputs was ```FieldSettings```  data type. 

```
data FieldSettings master = FieldSettings
    { fsLabel :: SomeMessage master
    , fsTooltip :: Maybe (SomeMessage master)
    , fsId :: Maybe Text
    , fsName :: Maybe Text
    , fsAttrs :: [(Text, Text)]
    }

```
You can use it to add label, tooltip or id to your input field but I didn't know how to use it but it is very simple:

```
 let emailSettings = FieldSettings {
                fsLabel = Just "My Label",
                fsTooltip = Nothing,
                fsId = Just "email",
                fsName = Just "email",
                fsAttrs = [("autofocus", "true"),("class","form-control")]
         }

Tutorial <$>  mreq emailField emailSettings Nothing

-- OR like this if you want to set just an id for example

Tutorial <$> areq textField "Title" {fsId = "user-defined-id} (Just "Default value")
```
Now your input will have id="user-defined-id" and predefined value of "Default value" instead of being empty. Nice. 

Make sure you checkout [bootstrap3](https://hackage.haskell.org/package/yesod-form-1.4.11/docs/Yesod-Form-Bootstrap3.html) on [Hackage](https://hackage.haskell.org)

#### What about the Handler ?

To actually display created form we will need a GET Handler. Here is the code :

```
-- Tutorial.hs
getTutorialsR :: Handler Html
getTutorialsR = do
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm tutorialForm
  defaultLayout $ do $(widgetFile "tutorials/new")
```

What I showed you was almost completely the same as the stuff you can see in the linked tutorial. One thing that is missing from that tutorial is how to do the edit and that is what I will cover next. 
You will need to use 
```
yesod add-handler
```
command to create new handler responsible for editing. To fill in form with the edit data:

```
tutorialForm
  :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
  => Tutorial -> AForm m Tutorial
tutorialForm tutorial =
  Tutorial <$>areq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial) 
          <*> areq markdownField (bfs ("Content" :: Text)) (Just $ tutorialContent tutorial)

getTutorialEditR :: TutorialId -> Handler Html
getTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  (widget, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  defaultLayout $ do $(widgetFile "tutorials/edit")

```
Notice what we do in this line
```
areq textField (bfs ("Title" :: Text )) (Just $ tutorialTitle tutorial)
```
We are using the record syntax generated function tutorialTitle to extract title from tutorial :: Tutorial and display it in form with Maybe constructor Just.
Once the form submit button is clicked this post handler gets called:

```
-- Handlers/TutorialEdit.hs

postTutorialEditR :: TutorialId -> Handler Html
postTutorialEditR tutorialId = do
  tutorial <- runDB . get404 $ tutorialId
  ((res, _), _) <-
    runFormPost $ renderBootstrap3 BootstrapBasicForm $ tutorialForm tutorial
  case res of
    FormSuccess tut -> do
      let edited =
            Tutorial
            { tutorialTitle = tutorialTitle tut
            , tutorialContent = tutorialContent tut
            }
      _ <- runDB $ update tutorialId [TutorialTitle =. tutorialTitle edited, TutorialContent =. tutorialContent edited]
      redirect $ TutorialRR tutorialId
    _ -> do
      setMessage "Tutorial not edited"
      redirect $ TutorialListR

```
This is pretty self explanatory like reading a book: 

*  Find tutorial by id or redirect to 404 not found page (that is what get404 does)
* Run the form and if it is successful create new Tutorial data type with values from the form (I am sure there is a shorter way to do this)
* Finally do the update and redirect to tutorial list page

I am leaving to you to create templates, that is fairly simple and you can always cheet and check out the source code for haskell-serbia on github.

**Pro Tip
When going trough libraries on Hackage scroll down to Modules section and there you can click on the Module to see the actual API 

![haskage modules](/static/img/hackage-modules.png "Hackage modules")

** by the way when you read Pro Tip it actually means beginner tip

Trick is to really get use to reading library code, look at the types and stars will align for you. If not - ask for help, haskell community id great.

### What next ?
Now you have complete example of doing a simple CRUD if you merge [video tutorial](https://www.youtube.com/watch?v=SadfV-qbVg8) and this piece of code. 
You can see all of this on [haskell-serbia repo](https://github.com/v0d1ch/haskell-serbia) on GitHub. Btw contributions are welcome ;) .

What is really annoying for me in Yesod is finding a way to override how the login and registration form looks so that is what I am going to cover next time.

Keep hacking!
