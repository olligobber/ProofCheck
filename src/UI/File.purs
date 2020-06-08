module UI.File
    ( readFile
    ) where

import Web.Event.Event (Event)
import Effect.Promise (Promise)

foreign import readFile :: Event -> Promise String
