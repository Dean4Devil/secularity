module Secularity (
    libmain
    ) where


import Control.Monad
import Data.Default
import Lens.Family2
import Network.Xmpp
import Network.Xmpp.Internal (TlsBehaviour(..))
import System.Log.Logger

libmain = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    result <- session "paranoidlabs.org"
                  (Just (\_ -> ([plain "test" Nothing "Vei8xee9ei"]),
                  Nothing))
                  $ def & streamConfigurationL . tlsBehaviourL .~ RequireTls
                        & onConnectionClosedL .~ reconnectSession
    sess <- case result of
                Right s -> return s
                Left e -> error $ "XMPP Failure: " ++ (show e)
    sendPresence def sess
    forever $ do
        msg <- getMessage sess
        case answerMessage msg (messagePayload msg) of
            Just answer -> sendMessage answer sess >> return ()
            Nothing -> putStrLn "Received message with no sender"
  where
      reconnectSession sess failure = reconnect' sess >> return ()
