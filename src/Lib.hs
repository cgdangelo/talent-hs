{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (showDemo) where

import Control.Exception (throw)
import Control.Monad (guard, replicateM)
import Data.Binary (Word16, Word32, Word8)
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as L
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word8)
import Debug.Trace (traceM)

data Point a = Point a a a deriving (Show)

data Demo = Demo DemoHeader Directory deriving (Show)

data DemoHeader = DemoHeader
  { magic :: L.ByteString,
    demoProtocol :: Word32,
    networkProtocol :: Word32,
    mapName :: L.ByteString,
    gameDirectory :: L.ByteString,
    mapChecksum :: Int32
  }
  deriving (Show)

type Directory = [DirectoryEntry]

data DirectoryEntry = DirectoryEntry
  { entryType :: Word32,
    description :: L.ByteString,
    flags :: Word32,
    cdTrack :: Int32,
    trackTime :: Float,
    framesCount :: Word32,
    framesOffset :: Word32,
    fileLength :: Word32,
    frames :: [Frame]
  }
  deriving (Show)

data Frame = Frame Float Int32 FrameData deriving (Show)

data FrameData
  = DemoStart
  | ConsoleCommand L.ByteString
  | ClientData (Point Float) (Point Float) Word32 Float
  | NextSection
  | Event Int32 Int32 Float EventArgs
  | WeaponAnimation Int32 Int32
  | Sound Int32 L.ByteString Float Float Int32 Int32
  | DemoBuffer [Word8]
  | NetworkMessages Float RefParams UserCmd MoveVars (Point Float) Int32 SequenceInfo [NetworkMessage]
  deriving (Show)

data EventArgs = EventArgs
  { flags :: Word32,
    entityIndex :: Word32,
    origin :: Point Float,
    angles :: Point Float,
    velocity :: Point Float,
    ducking :: Word32,
    fparam1 :: Float,
    fparam2 :: Float,
    iparam1 :: Int32,
    iparam2 :: Int32,
    bparam1 :: Int32,
    bparam2 :: Int32
  }
  deriving (Show)

data SequenceInfo = SequenceInfo
  { incomingSequence :: Int32,
    incomingAcknowledged :: Int32,
    incomingReliableAcknowledged :: Int32,
    incomingReliableSequence :: Int32,
    outgoingSequence :: Int32,
    reliableSequence :: Int32,
    lastReliableSequence :: Int32
  }
  deriving (Show)

data RefParams = RefParams
  { viewOrigin :: Point Float,
    viewAngles :: Point Float,
    forward :: Point Float,
    right :: Point Float,
    up :: Point Float,
    frameTime :: Float,
    time :: Float,
    intermission :: Int32,
    paused :: Int32,
    spectator :: Int32,
    onGround :: Int32,
    waterLevel :: Int32,
    simVel :: Point Float,
    simOrg :: Point Float,
    viewHeight :: Point Float,
    idealPitch :: Float,
    cl_viewangles :: Point Float,
    health :: Int32,
    crosshairAngle :: Point Float,
    viewSize :: Float,
    punchAngle :: Point Float,
    maxClients :: Int32,
    viewEntity :: Int32,
    playerNum :: Int32,
    maxEntities :: Int32,
    demoPlayback :: Int32,
    hardware :: Int32,
    smoothing :: Int32,
    ptrCmd :: Int32,
    ptrMoveVars :: Int32,
    viewPort :: (Int32, Int32, Int32, Int32),
    nextView :: Int32,
    onlyClientDraw :: Int32
  }
  deriving (Show)

data UserCmd = UserCmd
  { lerpMs :: Int16,
    ms :: Word8,
    viewAngles :: Point Float,
    forwardMove :: Float,
    sideMove :: Float,
    upMove :: Float,
    lightLevel :: Int8,
    buttons :: Word16,
    impulse :: Int8,
    weaponSelect :: Int8,
    impactIndex :: Int32,
    impactPosition :: Point Float
  }
  deriving (Show)

data MoveVars = MoveVars
  { gravity :: Float,
    stopSpeed :: Float,
    maxSpeed :: Float,
    spectatorMaxSpeed :: Float,
    accelerate :: Float,
    airAccelerate :: Float,
    waterAccelerate :: Float,
    friction :: Float,
    edgeFriction :: Float,
    waterFriction :: Float,
    entGravity :: Float,
    bounce :: Float,
    stepSize :: Float,
    maxVelocity :: Float,
    zMax :: Float,
    waveHeight :: Float,
    footsteps :: Int32,
    skyName :: L.ByteString,
    rollAngle :: Float,
    rollSpeed :: Float,
    skyColor :: (Float, Float, Float),
    skyVec :: Point Float
  }
  deriving (Show)

data NetworkMessage
  = SVC_BAD
  | SVC_NOP
  | SVC_DISCONNECT L.ByteString
  | SVC_EVENT
  | SVC_VERSION
  | SVC_SETVIEW
  | SVC_SOUND
  | SVC_TIME
  | SVC_PRINT L.ByteString
  | SVC_STUFFTEXT
  | SVC_SETANGLE
  | SVC_SERVERINFO
      { protocol :: Int32,
        spawnCount :: Int32,
        mapChecksum :: Int32,
        clientDllHash :: [Word8],
        maxPlayers :: Int8,
        playerIndex :: Int8,
        isDeathmatch :: Int8,
        gameDir :: L.ByteString,
        hostname :: L.ByteString,
        mapFileName :: L.ByteString,
        mapCycle :: L.ByteString
      }
  | SVC_LIGHTSTYLE
  | SVC_UPDATEUSERINFO
  | SVC_DELTADESCRIPTION
  | SVC_CLIENTDATA
  | SVC_STOPSOUND
  | SVC_PINGS
  | SVC_PARTICLE
  | SVC_DAMAGE
  | SVC_SPAWNSTATIC
  | SVC_EVENT_RELIABLE
  | SVC_SPAWNBASELINE
  | SVC_TEMPENTITY
  | SVC_SETPAUSE
  | SVC_SIGNONNUM
  | SVC_CENTERPRINT
  | SVC_KILLEDMONSTER
  | SVC_FOUNDSECRET
  | SVC_SPAWNSTATICSOUND
  | SVC_INTERMISSION
  | SVC_FINALE
  | SVC_CDTRACK
  | SVC_RESTORE
  | SVC_CUTSCENE
  | SVC_WEAPONANIM
  | SVC_DECALNAME
  | SVC_ROOMTYPE
  | SVC_ADDANGLE
  | SVC_NEWUSERMSG
  | SVC_PACKETENTITIES
  | SVC_DELTAPACKETENTITIES
  | SVC_CHOKE
  | SVC_RESOURCELIST
  | SVC_NEWMOVEVARS
  | SVC_RESOURCEREQUEST
  | SVC_CUSTOMIZATION
  | SVC_CROSSHAIRANGLE
  | SVC_SOUNDFADE
  | SVC_FILETXFERFAILED
  | SVC_HLTV
  | SVC_DIRECTOR
  | SVC_VOICEINIT
  | SVC_VOICEDATA
  | SVC_SENDEXTRAINFO L.ByteString Int8
  | SVC_TIMESCALE
  | SVC_RESOURCELOCATION
  | SVC_SENDCVARVALUE
  | SVC_SENDCVARVALUE2
  deriving (Show)

getPointBy :: B.Get a -> B.Get (Point a)
getPointBy f = Point <$> f <*> f <*> f

getPoint :: B.Get (Point Float)
getPoint = getPointBy B.getFloatle

getLazyByteStringNul :: Int64 -> B.Get L.ByteString
getLazyByteStringNul = fmap (L.takeWhile (/= 0)) . B.getLazyByteString

getDemo :: B.Get Demo
getDemo = Demo <$> getDemoHeader <*> getDirectory

getDemoHeader :: B.Get DemoHeader
getDemoHeader =
  DemoHeader
    <$> (getLazyByteStringNul 8 >>= (\x -> guard (x == "HLDEMO") $> x))
    <*> (B.getWord32le >>= (\x -> guard (x == 5) $> x))
    <*> B.getWord32le
    <*> getLazyByteStringNul 260
    <*> getLazyByteStringNul 260
    <*> B.getInt32le

getDirectory :: B.Get Directory
getDirectory =
  B.getWord32le
    >>= (B.lookAhead . getDirectoryEntries)
    >>= mapM addFramesToDirectoryEntry
  where
    getDirectoryEntries directoryOffset = do
      currentPosition <- fromIntegral <$> B.bytesRead
      B.skip $ fromIntegral $ directoryOffset - currentPosition
      totalEntries <- fromIntegral <$> B.getInt32le
      replicateM totalEntries getDirectoryEntry

    addFramesToDirectoryEntry a = do
      cur <- fromIntegral <$> B.bytesRead
      B.skip $ fromIntegral $ framesOffset a - cur
      (\x -> a {frames = x}) <$> getFrames

getDirectoryEntry :: B.Get DirectoryEntry
getDirectoryEntry =
  DirectoryEntry
    <$> B.getWord32le
    <*> getLazyByteStringNul 64
    <*> B.getWord32le
    <*> B.getInt32le
    <*> B.getFloatle
    <*> B.getWord32le
    <*> B.getWord32le
    <*> B.getWord32le
    <*> pure []

getFrames :: B.Get [Frame]
getFrames = do
  empty <- B.isEmpty
  if empty
    then pure []
    else do
      frame <- getFrame
      case frameData frame of
        NextSection -> pure []
        _ -> (frame :) <$> getFrames
  where
    frameData (Frame _ _ a) = a

getFrame :: B.Get Frame
getFrame = do
  frameType <- B.getInt8
  Frame
    <$> B.getFloatle
    <*> B.getInt32le
    <*> getFrameData frameType

getFrameData :: Int8 -> B.Get FrameData
getFrameData frameType = do
  guard $ frameType `elem` [0 .. 9]
  case frameType of
    0 -> getNetworkMessages
    1 -> getNetworkMessages
    2 -> pure DemoStart
    3 -> getConsoleCommand
    4 -> getClientData
    5 -> pure NextSection
    6 -> getEvent
    7 -> getWeaponAnimation
    8 -> getSound
    9 -> getDemoBuffer
    _ -> error "Invalid frame type"

getNetworkMessages =
  NetworkMessages
    <$> B.getFloatle
    <*> getRefParams
    <*> getUserCmd
    <*> getMoveVars
    <*> getPoint
    <*> B.getInt32le
    <*> getSequenceInfo
    <*> getMessagesInPacket

getRefParams =
  RefParams
    <$> getPoint
    <*> getPoint
    <*> getPoint
    <*> getPoint
    <*> getPoint
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> getPoint
    <*> getPoint
    <*> getPoint
    <*> B.getFloatle
    <*> getPoint
    <*> B.getInt32le
    <*> getPoint
    <*> B.getFloatle
    <*> getPoint
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> getViewPort
    <*> B.getInt32le
    <*> B.getInt32le
  where
    getViewPort =
      (,,,)
        <$> B.getInt32le
        <*> B.getInt32le
        <*> B.getInt32le
        <*> B.getInt32le

getUserCmd =
  UserCmd
    <$> B.getInt16le
    <*> (B.getWord8 <* B.skip 1)
    <*> getPoint
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> (B.getInt8 <* B.skip 1)
    <*> B.getWord16le
    <*> B.getInt8
    <*> (B.getInt8 <* B.skip 2)
    <*> B.getInt32le
    <*> getPoint

getMoveVars =
  MoveVars
    <$> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getInt32le
    <*> getLazyByteStringNul 32
    <*> B.getFloatle
    <*> B.getFloatle
    <*> ((,,) <$> B.getFloatle <*> B.getFloatle <*> B.getFloatle)
    <*> getPoint

getSequenceInfo =
  SequenceInfo
    <$> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le

getMessagesInPacket = B.getWord32le >>= B.skip . fromIntegral >> pure []

getConsoleCommand = ConsoleCommand <$> getLazyByteStringNul 64

getClientData =
  ClientData
    <$> getPoint
    <*> getPoint
    <*> B.getWord32le
    <*> B.getFloatle

getEvent =
  Event
    <$> B.getInt32le
    <*> B.getInt32le
    <*> B.getFloatle
    <*> getEventArgs

getEventArgs =
  EventArgs
    <$> B.getWord32le
    <*> B.getWord32le
    <*> getPoint
    <*> getPoint
    <*> getPoint
    <*> B.getWord32le
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le
    <*> B.getInt32le

getWeaponAnimation =
  WeaponAnimation
    <$> B.getInt32le
    <*> B.getInt32le

getSound =
  Sound
    <$> B.getInt32le
    <*> (B.getWord32le >>= B.getLazyByteString . fromIntegral)
    <*> B.getFloatle
    <*> B.getFloatle
    <*> B.getInt32le
    <*> B.getInt32le

getDemoBuffer =
  DemoBuffer
    <$> (B.getWord32le >>= flip replicateM B.getWord8 . fromIntegral)

showDemo :: IO ()
showDemo = L.readFile "/tmp/demo.dem" >>= print . B.runGet getDemo
