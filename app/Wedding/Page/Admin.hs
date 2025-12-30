module Wedding.Page.Admin (adminLogin, adminDashboard, adminLoginHandler, csvUploadHandler, deleteAttendeeHandler, createAttendeeHandler, exportCsvHandler, editAttendeePageHandler, editAttendeeFormHandler, CsvUpload, CreateAttendeeForm (..), EditAttendeeForm (..)) where

import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Csv (HasHeader (HasHeader), ToRecord, decode, encode, record, toField, toRecord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Vector qualified as V
import Effectful (Eff, (:>))
import Effectful.Error.Static (Error, throwError)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO.ByteString (readFile)
import GHC.Generics (Generic)
import Lucid
import Servant (Headers, ServerError, err401, err404)
import Servant.API (Header)
import Servant.Auth.Server (SetCookie)
import Servant.Multipart (FileData (fdInputName, fdPayload), MultipartData (files), Tmp)
import Web.FormUrlEncoded (FromForm)
import Wedding.Auth (AuthE, LoginForm (..), acceptLogin, getAdminPassword)
import Wedding.CSV (insertFromCsv)
import Wedding.Component.BasePage (basePage)
import Wedding.DB (Attendee (..), AttendingStatus (..), DB, createAttendee, deleteAttendee, getAllAttendees, getAttendeeById, updateAttendee)
import Prelude hiding (readFile)

-- | CSV upload form data
type CsvUpload = MultipartData Tmp

-- | Create attendee form data
data CreateAttendeeForm = CreateAttendeeForm
  { name :: Text,
    group :: Text
  }
  deriving (Generic)

instance FromForm CreateAttendeeForm

-- | Edit attendee form data
data EditAttendeeForm = EditAttendeeForm
  { editName :: Text,
    editGroup :: Text,
    editAttending :: Text,
    editDietaryRestrictions :: Text
  }
  deriving (Generic)

instance FromForm EditAttendeeForm

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

        -- Export CSV Button
        div_ [class_ "mb-6"] $ do
          a_ [href_ "/admin/export-csv", class_ "btn btn-success", download_ "guests.csv"] "Export Guest List to CSV"

        -- CSV Upload Section
        div_ [class_ "mb-6"] $ do
          div_ [class_ "bg-gray-50 p-4 rounded-lg"] $ do
            h2_ [class_ "text-xl font-semibold mb-2"] "CSV Upload"
            p_ [class_ "text-gray-600 mb-4"] "Upload guest list from CSV file"
            form_ [action_ "/admin/upload-csv", method_ "post", enctype_ "multipart/form-data"] $ do
              div_ [class_ "mb-3"] $
                input_ [type_ "file", name_ "csvFile", accept_ ".csv", class_ "form-control", required_ "required"]
              button_ [type_ "submit", class_ "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"] "Upload CSV"

        -- Create Attendee Form
        div_ [class_ "mb-6"] $ do
          div_ [class_ "bg-gray-50 p-4 rounded-lg"] $ do
            h2_ [class_ "text-xl font-semibold mb-2"] "Add New Attendee"
            form_ [action_ "/admin/create-attendee", method_ "post"] $ do
              div_ [class_ "row mb-3"] $ do
                div_ [class_ "col-md-5"] $ do
                  label_ [for_ "attendeeName", class_ "form-label"] "Name"
                  input_ [type_ "text", id_ "attendeeName", name_ "name", class_ "form-control", required_ "required"]
                div_ [class_ "col-md-5"] $ do
                  label_ [for_ "attendeeGroup", class_ "form-label"] "Group (optional)"
                  input_ [type_ "text", id_ "attendeeGroup", name_ "group", class_ "form-control"]
                div_ [class_ "col-md-2 d-flex align-items-end"] $
                  button_ [type_ "submit", class_ "btn btn-primary w-100"] "Add Attendee"

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
                      th_ [scope_ "col"] "Actions"
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
      td_ $ do
        a_
          [ href_ ("/admin/edit/" <> T.pack (show attendeeId)),
            class_ "btn btn-primary btn-sm me-2"
          ]
          "Edit"
        form_ [action_ ("/admin/delete/" <> T.pack (show attendeeId)), method_ "post", style_ "display: inline;"] $
          button_
            [ type_ "submit",
              class_ "btn btn-danger btn-sm",
              onclick_ "return confirm('Are you sure you want to delete this attendee?');"
            ]
            "Delete"

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

-- | Handle attendee deletion
deleteAttendeeHandler :: (DB :> es, Error ServerError :> es) => Int -> Eff es (Html ())
deleteAttendeeHandler attendeeId = do
  deleteAttendee attendeeId
  return $
    basePage "Delete Success" $
      div_ [class_ "container mt-5"] $ do
        div_ [class_ "alert alert-success"] "Attendee deleted successfully"
        script_ "setTimeout(function(){ window.location.href = '/admin'; }, 1000);"

-- | Handle attendee creation
createAttendeeHandler :: (DB :> es) => CreateAttendeeForm -> Eff es (Html ())
createAttendeeHandler (CreateAttendeeForm name group) = do
  let groupValue = if T.null group then Nothing else Just group
  createAttendee name groupValue
  return $
    basePage "Create Success" $
      div_ [class_ "container mt-5"] $ do
        div_ [class_ "alert alert-success"] "Attendee created successfully"
        script_ "setTimeout(function(){ window.location.href = '/admin'; }, 1000);"

-- | CSV export row with attending status and notes
data ExportCSVRow = ExportCSVRow Text (Maybe Text) Text (Maybe Text)

instance ToRecord ExportCSVRow where
  toRecord (ExportCSVRow name group attending dietary) =
    record
      [ toField name,
        toField $ fromMaybe "" group,
        toField attending,
        toField $ fromMaybe "" dietary
      ]

-- | Handle CSV export
exportCsvHandler :: (DB :> es) => Eff es Text
exportCsvHandler = do
  attendees <- getAllAttendees
  let header = "name,group,attending,dietary_restrictions\n"
  let exportRows = map attendeeToExportRow attendees
  let csvData = encode exportRows
  return $ decodeUtf8 $ toStrict $ fromStrict (encodeUtf8 header) <> csvData
  where
    attendeeToExportRow :: Attendee -> ExportCSVRow
    attendeeToExportRow (Attendee _ name group attending dietary) =
      ExportCSVRow name group (attendingToText attending) dietary

    attendingToText :: AttendingStatus -> Text
    attendingToText Yes = "Yes"
    attendingToText No = "No"
    attendingToText Undecided = "Undecided"

-- | Handle edit attendee page (GET)
editAttendeePageHandler :: (DB :> es, Error ServerError :> es) => Int -> Eff es (Html ())
editAttendeePageHandler attendeeId = do
  mAttendee <- getAttendeeById attendeeId
  case mAttendee of
    Nothing -> throwError err404
    Just attendee -> return $ adminEditPage attendee

-- | Edit attendee page
adminEditPage :: Attendee -> Html ()
adminEditPage (Attendee attendeeId attendeeName attendeeGroup attendeeAttending attendeeDietary) = basePage "Edit Attendee" $ do
  div_ [class_ "container mx-auto px-4 py-8"] $ do
    div_ [class_ "bg-white rounded-lg shadow-md p-6"] $ do
      h1_ [class_ "text-3xl font-bold mb-6"] "Edit Attendee"

      form_ [action_ ("/admin/edit/" <> T.pack (show attendeeId)), method_ "post"] $ do
        div_ [class_ "mb-4"] $ do
          label_ [for_ "editName", class_ "form-label"] "Name"
          input_
            [ type_ "text",
              id_ "editName",
              name_ "editName",
              class_ "form-control",
              value_ attendeeName,
              required_ "required"
            ]

        div_ [class_ "mb-4"] $ do
          label_ [for_ "editGroup", class_ "form-label"] "Group"
          input_
            [ type_ "text",
              id_ "editGroup",
              name_ "editGroup",
              class_ "form-control",
              value_ (fromMaybe "" attendeeGroup)
            ]

        div_ [class_ "mb-4"] $ do
          label_ [for_ "editAttending", class_ "form-label"] "Attending Status"
          select_
            [ id_ "editAttending",
              name_ "editAttending",
              class_ "form-select"
            ]
            $ do
              option_ (value_ "Yes" : [selected_ "selected" | attendeeAttending == Yes]) "Yes"
              option_ (value_ "No" : [selected_ "selected" | attendeeAttending == No]) "No"
              option_ (value_ "Undecided" : [selected_ "selected" | attendeeAttending == Undecided]) "Undecided"

        div_ [class_ "mb-4"] $ do
          label_ [for_ "editDietaryRestrictions", class_ "form-label"] "Dietary Restrictions"
          textarea_
            [ id_ "editDietaryRestrictions",
              name_ "editDietaryRestrictions",
              class_ "form-control",
              rows_ "3"
            ]
            (toHtml $ fromMaybe "" attendeeDietary)

        div_ [class_ "d-flex gap-2"] $ do
          button_ [type_ "submit", class_ "btn btn-primary"] "Save Changes"
          a_ [href_ "/admin", class_ "btn btn-secondary"] "Cancel"

-- | Handle edit attendee form submission (POST)
editAttendeeFormHandler :: (DB :> es, Error ServerError :> es) => Int -> EditAttendeeForm -> Eff es (Html ())
editAttendeeFormHandler attendeeId (EditAttendeeForm name group attendingText dietary) = do
  mAttendee <- getAttendeeById attendeeId
  case mAttendee of
    Nothing -> throwError err404
    Just _ -> do
      let groupValue = if T.null group then Nothing else Just group
      let dietaryValue = if T.null dietary then Nothing else Just dietary
      let attendingStatus = case attendingText of
            "Yes" -> Yes
            "No" -> No
            _ -> Undecided
      let updatedAttendee = Attendee attendeeId name groupValue attendingStatus dietaryValue
      updateAttendee updatedAttendee
      return $
        basePage "Edit Success" $
          div_ [class_ "container mt-5"] $ do
            div_ [class_ "alert alert-success"] "Attendee updated successfully"
            script_ "setTimeout(function(){ window.location.href = '/admin'; }, 1000);"
