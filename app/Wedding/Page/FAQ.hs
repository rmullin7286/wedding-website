module Wedding.Page.FAQ (faq) where

import Data.Text (Text)
import Lucid (Html, a_, class_, div_, h1_, h2_, href_, p_, section_)
import Wedding.Component.BasePage (basePage)

faq :: Html ()
faq = basePage "FAQ" $ do
  section_ [class_ "py-5"] $ do
    div_ [class_ "container"] $ do
      h1_ [class_ "display-4 text-center mb-5"] "Frequently Asked Questions"

      div_ [class_ "row g-4"] $ do
        faqCard "primary" "Dress Code" "Our dress code is semi-formal!"

        faqCard "secondary" "What time should I arrive?" "The ceremony will start at 3PM. Guests should arrive at 2:30 PM."

        faqCard "neutral" "Parking Information" "Parking is available on site. Parking attendants will be directing traffic as guests arrive. Try to carpool if possible. The venue is near Redmond proper, so Ubers or taxis will likely be available."

        faqCard "primary" "Where do I stay?" "We don't have any hotel blocks. There are plenty of hotels in the greater Seattle area. Feel free to reach out if you need any recommendations."

        faqCard "secondary" "Are kids allowed?" "Absolutely! We welcome family and friends of all ages to come celebrate our special day with us."

        faqCard
          "neutral"
          "Dietary Restrictions"
          ( "We'll make sure to accomadate dietary restrictions to the best of our ability in the food we serve.\n"
              <> "Just make sure to list any allergies in your RSVP response. If you need to amend your response, you can just resubmit it."
          )

        faqCard
          "primary"
          "What date should I RSVP by?"
          "RSVPs should be completed by March 28th so we know how many guests we can expect on the day of."

        faqCard
          "secondary"
          "Can I bring a plus one?"
          "Please bring only guests listed on the invitation. Feel free to reach out to us if needed."

        faqCardCentered "neutral" "Anything Else?" $ do
          "Feel free to "
          a_ [href_ "/#contact", class_ "faq-link"] "contact us"
          "."

faqCard :: Text -> Html () -> Html () -> Html ()
faqCard colorClass question answer = do
  div_ [class_ "col-md-6"] $ do
    div_ [class_ $ "faq-card faq-card-" <> colorClass] $ do
      h2_ [class_ "faq-question"] question
      p_ [class_ "faq-answer"] answer

faqCardCentered :: Text -> Html () -> Html () -> Html ()
faqCardCentered colorClass question answer = do
  div_ [class_ "col-md-6 offset-md-3"] $ do
    div_ [class_ $ "faq-card faq-card-" <> colorClass] $ do
      h2_ [class_ "faq-question"] question
      p_ [class_ "faq-answer"] answer
