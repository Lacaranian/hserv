{-# OPTIONS -Wall #-}
module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
--import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)

newtype Msg a = Msg (Int, a)

defaultTCPPort :: PortNumber
defaultTCPPort = 4242

main :: IO ()
main = do
    -- Create single write, multi-read channel
    chan <- newChan
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bind sock (SockAddrInet defaultTCPPort iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    _ <- forkIO $ fix $ \loop -> do
        Msg (_,msg) <- readChan chan
        putStrLn msg
        loop
    mainLoop sock chan 0


mainLoop :: Socket -> Chan (Msg String) -> Int -> IO ()
mainLoop sock chan nr = do
    -- accept one connection and handle it
    conn <- accept sock
    _ <- forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1

runConn :: (Socket, SockAddr) -> Chan (Msg String) -> Int -> IO ()
runConn (sock,_) chan nr = do
    let broadcast msg = writeChan chan (Msg (nr,msg))
    --Turn the socket into a handle for ease of use
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    --Set username for new user, declare entrance
    hPutStrLn hdl "Hi, what's your name?"
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    --Duplicate the channel - writes to chan will be visible from chan'
    chan' <- dupChan chan
    -- fork off thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        Msg (nr',line) <- readChan chan'
        --You don't see your own broadcasts
        when (nr /= nr') $ hPutStrLn hdl line
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        case line of
            "quit" -> hPutStrLn hdl "Bye!"
            _      -> do
                broadcast (name ++ ": " ++ line)
                loop
    -- read lines from socket and echo them back to the user
    killThread reader
    broadcast ("<-- " ++ name ++ "left.")
    hClose hdl
