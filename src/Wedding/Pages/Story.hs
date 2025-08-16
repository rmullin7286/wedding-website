module Wedding.Pages.Story (storyPage) where

import Lucid
import Wedding.Pages.Home (baseTemplate)

-- | Our Story page
storyPage :: Html ()
storyPage = baseTemplate "Our Story" $ do
  section_ [class_ "story-hero"] $ do
    h1_ "Our Story"
    p_ [class_ "story-subtitle"] "How two hearts became one"
  
  section_ [class_ "timeline"] $ do
    h2_ "Our Journey Together"
    div_ [class_ "timeline-container"] $ do
      timelineItem "2019" "First Meeting" 
        "We met at a coffee shop in downtown Portland. Ryan spilled coffee on Shae's laptop, and the rest is history!"
      timelineItem "2020" "First Date"
        "Despite the pandemic, we had our first official date at a socially distanced picnic in the park."
      timelineItem "2021" "Moving In"
        "We decided to take the next step and move in together. Our first apartment was tiny but perfect."
      timelineItem "2022" "The Adventure Begins"
        "We took our first big trip together - a cross-country road trip that solidified our bond."
      timelineItem "2023" "The Proposal"
        "Ryan proposed at the same coffee shop where we first met. Shae said yes (after making him clean up his mess first)!"
      timelineItem "2026" "The Big Day"
        "Here we are, ready to celebrate our love with all of you!"

-- | Timeline item component
timelineItem :: Html () -> Html () -> Html () -> Html ()
timelineItem year title description = div_ [class_ "timeline-item"] $ do
  div_ [class_ "timeline-year"] year
  div_ [class_ "timeline-content"] $ do
    h3_ title
    p_ description