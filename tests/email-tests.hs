module Main (main) where

import Control.Monad.Email
import Test.Hspec
import Text.Hamlet (shamlet)

main :: IO ()
main = putStrLn "test suite not implemented yet"

melbox :: Mailbox
melbox = [mailbox|Melanie Brown <brown.m@pm.me>|]

testmsg :: Mail
testmsg =
    newmessage melbox
        & to [melbox]
        & subject "Testing out package smtp-hs"
        & body
            [shamlet|
                <html>
                  <style>
                    .green {
                      background-color: #009fff;
                    }
                  <body>
                    <p>Test content.
                    <p .green>Please ignore.
            |]
