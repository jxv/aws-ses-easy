{-# LANGUAGE OverloadedStrings #-}
module Aws.Ses.Easy.Email
  ( Email(..)
  , fromHtml
  , center
  , blackFont
  , fromEmail
  ) where

import qualified Data.Text.Lazy as TL
import qualified Network.AWS.SES as Aws
import Control.Lens
import Data.Text (Text)
import Lucid
import Lucid.Base

data Email = Email
  { emailTo :: [Text]
  , emailFrom :: Text
  , emailTitle :: Text
  , emailBody :: Html ()
  } deriving (Show)

fromHtml :: Html a -> Text
fromHtml = TL.toStrict . renderText

center :: Html () -> Html ()
center chilens = do
    table_ [width_ "100%", makeAttribute "border" "0", makeAttribute "cellspacing" "0", makeAttribute "cellpadding" "0"] $
      tr_ $
        td_ [makeAttribute "align" "center"] $
          chilens

blackFont :: [Attribute] -> Html () -> Html ()
blackFont attributes chilens = term "font" (makeAttribute "color" "black" : attributes) chilens

fromEmail :: Email -> Aws.SendEmail
fromEmail e = Aws.sendEmail from' dest msg
  where
    from' = emailFrom e
    dest = Aws.destination & Aws.dToAddresses .~ (emailTo e)
    content = Aws.content $ fromHtml (emailBody e)
    title = Aws.content $ emailTitle e
    msg = Aws.message title (Aws.body & Aws.bHTML .~ (Just content))
