module Main
  ( main
  )
where

import           Control.Concurrent
import           Control.Monad

import           Network.Socket
import           Network.Socket.ByteString      ( recv
                                                , send
                                                )

bufferSize :: Int
bufferSize = 32768

listenLimit :: Int
listenLimit = 10

echoServerPort :: String
echoServerPort = "8888"

echoServer :: String -> IO ()
echoServer port = withSocketsDo $ do
  server <- createListener
  forever $ handleServer server
 where
  createListener :: IO Socket
  createListener = do
    addrinfo <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                            Nothing
                            (Just port)
    let addr = head addrinfo
    server <- socket (addrFamily addr) Stream defaultProtocol
    setSocketOption server ReuseAddr 1
    bind server (addrAddress addr)
    listen server listenLimit
    return server

  handleServer :: Socket -> IO ()
  handleServer server = do
    (client, addr) <- accept server
    putStrLn ("client connected:" ++ show addr)
    _ <- spawnClient client
    return ()

  handleClient :: Socket -> IO ()
  handleClient client = do
    input <- recv client bufferSize
    res   <- send client input
    when (res > 0) $ handleClient client

  spawnClient :: Socket -> IO ThreadId
  spawnClient client = forkIO $ handleClient client

main :: IO ()
main = echoServer echoServerPort
