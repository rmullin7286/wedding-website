module Wedding.Page.OurStory (ourStory) where

import Data.Text (Text)
import Lucid (Html, alt_, class_, div_, h1_, h2_, img_, p_, section_, span_, src_, sup_)
import Wedding.Component.BasePage (basePage)

ourStory :: Html ()
ourStory = basePage "Our Story" $ do
  posterSection
  ladderRow
    "our-story-bg-primary"
    True
    ("curated/jeopardy.jpg", "The Halloween party where it all began", "")
    "★★★★★"
    "Couple of the year"
    "New York Times"
  ladderRow
    "our-story-bg-secondary"
    False
    ("curated/alaska.jpg", "Glaciers in Alaska", "our-story-photo-bottom")
    "★★★★★"
    "Absolute Cinema"
    "Roger Ebert's Ghost"
  ladderRow
    "our-story-bg-neutral"
    True
    ("curated/kraken.jpg", "Go Kraken!", "our-story-photo-contain")
    "★★★★★"
    "A must see film for any Kraken fan"
    "Buoy"
  ladderRow
    "our-story-bg-primary"
    False
    ("curated/LOST.jpg", "DHARMA Initiative recruits", "")
    "★★★★★"
    "Excellent costuming. I especially loved the LOST references."
    "Hugo Reyes"
  ladderRow
    "our-story-bg-secondary"
    True
    ("curated/engaged.jpg", "The happy couple, newly engaged", "our-story-photo-contain")
    "★★★★★"
    "I can't wait to see where this series goes from here"
    $ do "AMC Stubs Premiere"; sup_ "TM"; " Member"
  closingSection

posterSection :: Html ()
posterSection =
  section_ [class_ "py-5 our-story-poster-section"] $
    div_ [class_ "container"] $
      div_ [class_ "row align-items-center g-5"] $ do
        div_ [class_ "col-md-5 text-center"] $
          img_ [src_ "/static/image/our-story/Save%20the%20Date.png", alt_ "Save the Date - Movie Poster", class_ "img-fluid our-story-poster"]
        div_ [class_ "col-md-7 text-center"] $ do
          p_ [class_ "our-story-studio"] "A Mullin-Huot Production"
          h1_ [class_ "our-story-title display-2 fw-bold"] "Our Story"
          div_ [class_ "our-story-rating mb-4"] $
            span_ [class_ "our-story-stars"] "★★★★★"
          h2_ [class_ "our-story-section-heading mb-4"] "Synopsis"
          p_ [class_ "our-story-blurb"] "Wenatchee's premier Software Developer, Ryan Mullin, has a chance encounter at a Halloween party with the woman of his dreams, Rocket Scientist Shaelyn Huot, as they discuss their deepest fears over a bowl of roasted pumpkin seeds. After a first date at the Kirkland Marina, full of Minigolf and Bananagrams, Ryan is quickly swept off his feet into a love that will last a lifetime."
          p_ [class_ "our-story-blurb"] "What follows is a journey that spans the globe, from Seattle to New York, to seeing the Beach Boys in Walla Walla, to White Water rafting through the Alaskan Frontier. As they see the world and learn more about each other, they come to understand how their fates are intertwined, living parallel lives in the same small town of Pullman, sharing many events and memories but never overlapping, until now."
          p_ [class_ "our-story-blurb"] "After a beautiful night in the Christmas village of Leavenworth, and a question on bended knee, our heroes are forced to confront the trials and tribulations of wedding planning. Once overcome, a love story for the ages awaits."

ladderRow :: Text -> Bool -> (Text, Text, Text) -> Html () -> Html () -> Html () -> Html ()
ladderRow bgClass photoFirst (file, altText, photoClass) stars quote critic =
  section_ [class_ $ "py-5 " <> bgClass] $
    div_ [class_ "container"] $
      div_ [class_ "row align-items-center g-4"] $
        if photoFirst
          then do
            photoCol file altText photoClass
            reviewCol stars quote critic
          else do
            reviewCol stars quote critic
            photoCol file altText photoClass

photoCol :: Text -> Text -> Text -> Html ()
photoCol file altText extraClass =
  div_ [class_ "col-md-6"] $
    div_ [class_ $ "our-story-photo " <> extraClass] $
      img_ [src_ $ "/static/image/our-story/" <> file, alt_ altText, class_ "img-fluid"]

reviewCol :: Html () -> Html () -> Html () -> Html ()
reviewCol stars quote critic =
  div_ [class_ "col-md-6 d-flex align-items-center justify-content-center"] $
    div_ [class_ "our-story-review-card"] $ do
      div_ [class_ "our-story-review-stars"] stars
      p_ [class_ "our-story-review-quote"] $ do
        "\""
        quote
        "\""
      p_ [class_ "our-story-review-critic"] $ do
        "— "
        critic

closingSection :: Html ()
closingSection =
  section_ [class_ "py-4 our-story-closing text-center"] $
    div_ [class_ "container"] $
      p_ [class_ "our-story-closing-text"] "To Be Continued..."
