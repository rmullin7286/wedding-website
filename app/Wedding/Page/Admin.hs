module Wedding.Page.Admin (adminLogin, adminDashboard, adminLoginHandler, csvUploadHandler, CsvUpload) where

import Data.ByteString.Lazy (fromStrict)
import Data.Csv (HasHeader (HasHeader), decode)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString (readFile)
import Lucid
import Servant (Headers, ServerError, err401)
import Servant.API (Header)
import Servant.Auth.Server (SetCookie)
import Servant.Multipart (FileData (fdInputName, fdPayload), MultipartData (files), Tmp)
import Wedding.Auth (AuthE, LoginForm (..), acceptLogin, getAdminPassword)
import Wedding.CSV (insertFromCsv)
import Wedding.Component.BasePage (basePage)
import Wedding.DB (Attendee (..), AttendingStatus (..), DB, getAllAttendees)
import Prelude hiding (readFile)

-- | CSV upload form data
type CsvUpload = MultipartData Tmp

-- | Admin login form page
adminLogin :: Html ()
adminLogin = basePage "Admin Login" $ do
  div_ [class_ "container", style_ "padding-top: 4rem; padding-bottom: 4rem;"] $ do
    div_ [class_ "row justify-content-center"] $ do
      div_ [class_ "col-md-6 col-lg-4"] $ do
        div_ [class_ "card shadow"] $ do
          div_ [class_ "card-body p-4"] $ do
            h1_ [class_ "h2 fw-bold mb-4 text-center"] "Admin Login"
            form_ [method_ "post", action_ "/admin/login"] $ do
              div_ [class_ "mb-4"] $ do
                label_ [for_ "password", class_ "form-label fw-medium", style_ "margin-bottom: 0.75rem;"] "Password"
                input_
                  [ type_ "password",
                    id_ "password",
                    name_ "password",
                    class_ "form-control",
                    required_ "required"
                  ]
              div_ [class_ "d-grid"] $
                button_
                  [ type_ "submit",
                    class_ "btn text-white fw-bold",
                    style_ "background-color: rgba(122,39,61,255);"
                  ]
                  "Login"

-- | Admin dashboard page
adminDashboard :: (DB :> es) => Eff es (Html ())
adminDashboard = do
  attendees <- getAllAttendees
  return $ basePage "Admin Dashboard" $ do
    div_ [class_ "container mx-auto px-4 py-8"] $ do
      div_ [class_ "bg-white rounded-lg shadow-md p-6"] $ do
        h1_ [class_ "text-3xl font-bold mb-6"] "Admin Dashboard"

        -- CSV Upload Section
        div_ [class_ "mb-6"] $ do
          div_ [class_ "bg-gray-50 p-4 rounded-lg"] $ do
            h2_ [class_ "text-xl font-semibold mb-2"] "CSV Upload"
            p_ [class_ "text-gray-600 mb-4"] "Upload guest list from CSV file"
            form_ [action_ "/admin/upload-csv", method_ "post", enctype_ "multipart/form-data"] $ do
              div_ [class_ "mb-3"] $
                input_ [type_ "file", name_ "csvFile", accept_ ".csv", class_ "form-control", required_ "required"]
              button_ [type_ "submit", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"] "Upload CSV"

        -- Attendees Table Section
        div_ [class_ "mb-6"] $ do
          h2_ [class_ "text-xl font-semibold mb-4"] "Current Attendees"
          if null attendees
            then div_ [class_ "alert alert-info"] "No attendees found. Upload a CSV file to add guests."
            else do
              div_ [class_ "table-responsive"] $ do
                table_ [class_ "table table-striped table-hover"] $ do
                  thead_ [class_ "table-dark"] $ do
                    tr_ $ do
                      th_ [scope_ "col"] "ID"
                      th_ [scope_ "col"] "Name"
                      th_ [scope_ "col"] "Group"
                      th_ [scope_ "col"] "Attending"
                      th_ [scope_ "col"] "Dietary Restrictions"
                  tbody_ $ do
                    mapM_ attendeeRow attendees
  where
    attendeeRow :: Attendee -> Html ()
    attendeeRow (Attendee attendeeId attendeeName attendeeGroup attendeeAttending attendeeDietary) = tr_ $ do
      td_ $ toHtml $ show attendeeId
      td_ $ toHtml attendeeName
      td_ $ maybe (em_ "No group") toHtml attendeeGroup
      td_ $ case attendeeAttending of
        Yes -> span_ [class_ "badge bg-success"] "Yes"
        No -> span_ [class_ "badge bg-danger"] "No"
        Undecided -> span_ [class_ "badge bg-warning text-dark"] "Undecided"
      td_ $ case attendeeDietary of
        Nothing -> span_ [class_ "badge bg-secondary"] "None specified"
        Just restrictions -> span_ [class_ "badge bg-info"] $ toHtml restrictions

-- | Handle login form submission
adminLoginHandler :: (AuthE :> es, Error ServerError :> es) => LoginForm -> Eff es (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] (Html ()))
adminLoginHandler (LoginForm submittedPassword) = do
  adminPassword <- getAdminPassword
  if submittedPassword == adminPassword
    then do
      mApplyCookies <- acceptLogin
      case mApplyCookies of
        Nothing -> throwError err401
        Just applyCookies -> do
          let loginSuccessHtml = div_ $ do
                h1_ "Login Successful"
                p_ "Redirecting to admin dashboard..."
                script_ "setTimeout(function(){ window.location.href = '/admin'; }, 2000);"
          return $ applyCookies loginSuccessHtml
    else throwError err401

-- | Handle CSV file upload
csvUploadHandler :: (FileSystem :> es, DB :> es) => CsvUpload -> Eff es (Html ())
csvUploadHandler multipartData = do
  let csvFiles = filter (\fd -> fdInputName fd == "csvFile") (files multipartData)
  case csvFiles of
    [] -> return $ errorPage ("No file uploaded" :: Text)
    (fileData : _) -> do
      let filePath = fdPayload fileData
      csvContent <- readFile filePath
      case decode HasHeader (fromStrict csvContent) of
        Left err -> return $ errorPage $ ("CSV parsing error: " :: Text) <> T.pack (show err)
        Right rows -> do
          let rowsList = V.toList rows
          insertFromCsv rowsList
          return $ successPage $ ("Successfully uploaded " :: Text) <> T.pack (show (length rowsList)) <> " guests"
  where
    errorPage msg = basePage "Upload Error" $
      div_ [class_ "container mt-5"] $ do
        div_ [class_ "alert alert-danger"] $ toHtml msg
        a_ [href_ "/admin", class_ "btn btn-primary"] "Back to Admin"

    successPage msg = basePage "Upload Success" $
      div_ [class_ "container mt-5"] $ do
        div_ [class_ "alert alert-success"] $ toHtml msg
        a_ [href_ "/admin", class_ "btn btn-primary"] "Back to Admin"
