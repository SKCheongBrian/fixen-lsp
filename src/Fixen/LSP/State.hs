module Fixen.LSP.State (
  ServerState,
  newServerState,
  lookupDocument,
  storeDocument,
  deleteDocument,
) where

import Data.IORef qualified as IORef
import Data.Map.Strict qualified as Map
import Language.LSP.Protocol.Types (Uri)

import Fixen.LSP.Types (DocumentAnalysis)

newtype ServerState = 
  ServerState 
    (IORef.IORef (Map.Map Uri DocumentAnalysis))

newServerState :: IO ServerState
newServerState =
  ServerState <$> IORef.newIORef Map.empty

lookupDocument
  :: ServerState
  -> Uri
  -> IO (Maybe DocumentAnalysis)
lookupDocument (ServerState state) uri =
  Map.lookup uri <$> IORef.readIORef state

storeDocument
  :: ServerState
  -> Uri
  -> DocumentAnalysis
  -> IO ()
storeDocument (ServerState state) uri analysis =
  IORef.atomicModifyIORef' state $ \cache ->
    (Map.insert uri analysis cache, ())

deleteDocument
  :: ServerState
  -> Uri
  -> IO ()
deleteDocument (ServerState state) uri =
  IORef.atomicModifyIORef' state $ \cache ->
    (Map.delete uri cache, ())
