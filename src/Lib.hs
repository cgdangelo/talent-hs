{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (showDemo) where

import Control.Exception (throw)
import Control.Monad (replicateM)
import Data.Binary
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as L
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word
import Debug.Trace

data Point a = Point a a a deriving (Show)

pointBy :: B.Get a -> B.Get (Point a)
pointBy f = Point <$> f <*> f <*> f

pointFloat = pointBy B.getFloatle

getPaddedLazyByteStringNul :: Int64 -> B.Get L.ByteString
getPaddedLazyByteStringNul = fmap (L.takeWhile (/= 0)) . B.getLazyByteString

data Demo = Demo
  { demoHeader :: DemoHeader,
    directory :: Directory
  }
  deriving (Show)

data DemoHeader = DemoHeader
  { magic :: L.ByteString,
    demoProtocol :: Int32,
    networkProtocol :: Int32,
    mapName :: L.ByteString,
    gameDirectory :: L.ByteString,
    mapChecksum :: Int32,
    directoryOffset :: Int32
  }
  deriving (Show)

type Directory = [DirectoryEntry]

data DirectoryEntry = DirectoryEntry
  { entryType :: DirectoryEntryType,
    description :: L.ByteString,
    flags :: Int32,
    cdTrack :: Int32,
    trackTime :: Float,
    framesCount :: Int32,
    framesOffset :: Int32,
    fileLength :: Int32,
    frames :: [Frame]
  }
  deriving (Show)

data DirectoryEntryType
  = Loading
  | Playback
  | UnknownDirectoryType
  deriving (Show)

data Frame = Frame
  { frameHeader :: FrameHeader,
    frameData :: FrameData
  }
  deriving (Show)

data FrameHeader = FrameHeader
  { time :: Float,
    frame :: Int32
  }
  deriving (Show)

data FrameData
  = DemoStart
  | ConsoleCommand L.ByteString
  | ClientData
      { origin :: Point Float,
        viewAngles :: Point Float,
        weaponBits :: Int32,
        fov :: Float
      }
  | NextSection
  | Event
      { flags :: Int32,
        index :: Int32,
        delay :: Float,
        args :: EventArgs
      }
  | WeaponAnimation
      { animation :: Int32,
        body :: Int32
      }
  | Sound
      { channel :: Int32,
        sample :: L.ByteString,
        attenuation :: Float,
        volume :: Float,
        flags :: Int32,
        pitch :: Int32
      }
  | DemoBuffer [Word8]
  | NetworkMessages
      { timestamp :: Float,
        refParams :: RefParams,
        userCmd :: UserCmd,
        moveVars :: MoveVars,
        view :: Point Float,
        viewModel :: Int32,
        messagesLength :: Int32,
        messages :: [NetworkMessage]
      }
  | UnknownFrameType
  deriving (Show)

data EventArgs = EventArgs
  { flags :: Int32,
    entityIndex :: Int32,
    origin :: Point Float,
    angles :: Point Float,
    velocity :: Point Float,
    ducking :: Int32,
    fparam1 :: Float,
    fparam2 :: Float,
    iparam1 :: Int32,
    iparam2 :: Int32,
    bparam1 :: Int32,
    bparam2 :: Int32
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
    ms :: Int8,
    viewAngles :: Point Float,
    forwardMove :: Float,
    sideMove :: Float,
    upMove :: Float,
    lightLevel :: Int8,
    buttons :: Int16,
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
    skyColor :: (Int32, Int32, Int32),
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
  | SVC_SENDEXTRAINFO
      { fallbackDir :: L.ByteString,
        canCheat :: Int8
      }
  | SVC_TIMESCALE
  | SVC_RESOURCELOCATION
  | SVC_SENDCVARVALUE
  | SVC_SENDCVARVALUE2
  deriving (Show)

getDemo :: B.Get Demo
getDemo = do
  demoHeader <- getDemoHeader
  directory <- B.lookAhead (getDirectory $ directoryOffset demoHeader) >>= addFramesToDirectoryEntries
  return Demo {..}
  where
    addFramesToDirectoryEntries = mapM addFramesToDirectoryEntry

    addFramesToDirectoryEntry :: DirectoryEntry -> B.Get DirectoryEntry
    addFramesToDirectoryEntry de = do
      currentPosition <- fromIntegral <$> B.bytesRead
      B.skip $ fromIntegral (framesOffset de) - currentPosition
      directoryEntryFrames <- getFrames
      pure de {frames = directoryEntryFrames}

getDemoHeader :: B.Get DemoHeader
getDemoHeader = do
  magic <- getPaddedLazyByteStringNul 8
  demoProtocol <- B.getInt32le
  networkProtocol <- B.getInt32le
  mapName <- getPaddedLazyByteStringNul 260
  gameDirectory <- getPaddedLazyByteStringNul 260
  mapChecksum <- B.getInt32le
  directoryOffset <- B.getInt32le
  return DemoHeader {..}

getDirectory :: Int32 -> B.Get Directory
getDirectory directoryOffset = do
  currentPosition <- B.bytesRead
  B.skip $ fromIntegral $ directoryOffset - fromIntegral currentPosition
  totalEntries <- B.getInt32le
  getDirectoryEntries $ fromIntegral totalEntries

getDirectoryEntries :: Int -> B.Get [DirectoryEntry]
getDirectoryEntries = flip replicateM getDirectoryEntry

getDirectoryEntry :: B.Get DirectoryEntry
getDirectoryEntry = do
  entryType <- getDirectoryEntryType
  description <- getPaddedLazyByteStringNul 64
  flags <- B.getInt32le
  cdTrack <- B.getInt32le
  trackTime <- B.getFloatle
  framesCount <- B.getInt32le
  framesOffset <- B.getInt32le
  fileLength <- B.getInt32le
  let frames = []
  -- frames <- getFrames
  return DirectoryEntry {..}
  where
    getDirectoryEntryType :: B.Get DirectoryEntryType
    getDirectoryEntryType = do
      directoryEntryType <- B.getInt32le
      pure $ case directoryEntryType of
        0 -> Loading
        1 -> Playback
        _ -> UnknownDirectoryType

getFrames :: B.Get [Frame]
getFrames = do
  empty <- B.isEmpty
  if empty
    then return []
    else do
      frame <- getFrame
      case frameData frame of
        NextSection -> pure []
        _ -> do
          frames <- getFrames
          return (frame : frames)

getFrame :: B.Get Frame
getFrame = do
  frameType <- B.getInt8
  frameHeader <- getFrameHeader
  frameData <- case frameType of
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
    _ -> pure UnknownFrameType
  return Frame {..}
  where
    getFrameHeader = FrameHeader <$> B.getFloatle <*> B.getInt32le

getConsoleCommand = ConsoleCommand <$> getPaddedLazyByteStringNul 64

getClientData = do
  origin <- pointFloat
  viewAngles <- pointFloat
  weaponBits <- B.getInt32le
  fov <- B.getFloatle
  return ClientData {..}

getEvent = do
  flags <- B.getInt32le
  index <- B.getInt32le
  delay <- B.getFloatle
  args <- getEventArgs
  return Event {..}

getEventArgs = do
  flags <- B.getInt32le
  entityIndex <- B.getInt32le
  origin <- pointFloat
  angles <- pointFloat
  velocity <- pointFloat
  ducking <- B.getInt32le
  fparam1 <- B.getFloatle
  fparam2 <- B.getFloatle
  iparam1 <- B.getInt32le
  iparam2 <- B.getInt32le
  bparam1 <- B.getInt32le
  bparam2 <- B.getInt32le
  return EventArgs {..}

getWeaponAnimation = do
  animation <- B.getInt32le
  body <- B.getInt32le
  return WeaponAnimation {..}

getSound = do
  channel <- B.getInt32le
  sample <- getSample
  attenuation <- B.getFloatle
  volume <- B.getFloatle
  flags <- B.getInt32le
  pitch <- B.getInt32le
  return Sound {..}
  where
    getSample = B.getInt64le >>= B.getLazyByteString

getDemoBuffer = do
  bufferLength <- B.getInt32le
  DemoBuffer <$> replicateM (fromIntegral bufferLength) B.getWord8

getNetworkMessages = do
  timestamp <- B.getFloatle
  refParams <- getRefParams
  userCmd <- getUserCmd
  moveVars <- getMoveVars
  view <- pointFloat
  viewModel <- B.getInt32le
  incomingSequence <- B.getInt32le
  incomingAcknowledged <- B.getInt32le
  incomingReliableAcknowledged <- B.getInt32le
  incomingReliableSequence <- B.getInt32le
  outgoingSequence <- B.getInt32le
  reliableSequence <- B.getInt32le
  lastReliableSequence <- B.getInt32le
  messagesLength <- B.getInt32le
  -- B.skip $ fromIntegral messagesLength
  messages <- getNetworkMessagesInPacket messagesLength
  return NetworkMessages {..}

getRefParams :: B.Get RefParams
getRefParams = do
  viewOrigin <- pointFloat
  viewAngles <- pointFloat
  forward <- pointFloat
  right <- pointFloat
  up <- pointFloat
  frameTime <- B.getFloatle
  time <- B.getFloatle
  intermission <- B.getInt32le
  paused <- B.getInt32le
  spectator <- B.getInt32le
  onGround <- B.getInt32le
  waterLevel <- B.getInt32le
  simVel <- pointFloat
  simOrg <- pointFloat
  viewHeight <- pointFloat
  idealPitch <- B.getFloatle
  cl_viewangles <- pointFloat
  health <- B.getInt32le
  crosshairAngle <- pointFloat
  viewSize <- B.getFloatle
  punchAngle <- pointFloat
  maxClients <- B.getInt32le
  viewEntity <- B.getInt32le
  playerNum <- B.getInt32le
  maxEntities <- B.getInt32le
  demoPlayback <- B.getInt32le
  hardware <- B.getInt32le
  smoothing <- B.getInt32le
  ptrCmd <- B.getInt32le
  ptrMoveVars <- B.getInt32le
  viewPort <- getViewPort
  nextView <- B.getInt32le
  onlyClientDraw <- B.getInt32le
  return RefParams {..}
  where
    getViewPort =
      (,,,)
        <$> B.getInt32le
        <*> B.getInt32le
        <*> B.getInt32le
        <*> B.getInt32le

getUserCmd :: B.Get UserCmd
getUserCmd = do
  lerpMs <- B.getInt16le
  ms <- B.getInt8
  viewAngles <- B.skip 1 *> pointFloat
  forwardMove <- B.getFloatle
  sideMove <- B.getFloatle
  upMove <- B.getFloatle
  lightLevel <- B.getInt8
  buttons <- B.skip 1 *> B.getInt16le
  impulse <- B.getInt8
  weaponSelect <- B.getInt8
  impactIndex <- B.skip 2 *> B.getInt32le
  impactPosition <- pointFloat
  return UserCmd {..}

getMoveVars :: B.Get MoveVars
getMoveVars = do
  gravity <- B.getFloatle
  stopSpeed <- B.getFloatle
  maxSpeed <- B.getFloatle
  spectatorMaxSpeed <- B.getFloatle
  accelerate <- B.getFloatle
  airAccelerate <- B.getFloatle
  waterAccelerate <- B.getFloatle
  friction <- B.getFloatle
  edgeFriction <- B.getFloatle
  waterFriction <- B.getFloatle
  entGravity <- B.getFloatle
  bounce <- B.getFloatle
  stepSize <- B.getFloatle
  maxVelocity <- B.getFloatle
  zMax <- B.getFloatle
  waveHeight <- B.getFloatle
  footsteps <- B.getInt32le
  skyName <- getPaddedLazyByteStringNul 32
  rollAngle <- B.getFloatle
  rollSpeed <- B.getFloatle
  skyColor <- getSkyColor
  skyVec <- pointFloat
  return MoveVars {..}
  where
    getSkyColor =
      (,,)
        <$> B.getInt32le
        <*> B.getInt32le
        <*> B.getInt32le

getNetworkMessagesInPacket :: Int32 -> B.Get [NetworkMessage]
getNetworkMessagesInPacket messagesLength = do
  currentPosition <- fromIntegral <$> B.bytesRead
  let messagesEndAt = currentPosition + messagesLength
  getNetworkMessagesInPacket' messagesEndAt
  where
    getNetworkMessagesInPacket' :: Int32 -> B.Get [NetworkMessage]
    getNetworkMessagesInPacket' endPosition = do
      currentPosition <- fromIntegral <$> B.bytesRead

      if currentPosition == endPosition
        then return []
        else do
          messageType <- B.getInt8
          message <- case messageType of
            0 -> pure SVC_BAD
            1 -> pure SVC_NOP
            2 -> SVC_DISCONNECT <$> B.getLazyByteStringNul
            3 -> pure SVC_EVENT
            4 -> pure SVC_VERSION
            5 -> pure SVC_SETVIEW
            6 -> pure SVC_SOUND
            7 -> pure SVC_TIME
            8 -> SVC_PRINT <$> B.getLazyByteStringNul
            9 -> pure SVC_STUFFTEXT
            10 -> pure SVC_SETANGLE
            11 -> do
              protocol <- B.getInt32le
              spawnCount <- B.getInt32le
              mapChecksum <- B.getInt32le
              clientDllHash <- B.skip 16 $> []
              maxPlayers <- B.getInt8
              playerIndex <- B.getInt8
              isDeathmatch <- B.getInt8
              gameDir <- B.getLazyByteStringNul
              hostname <- B.getLazyByteStringNul
              mapFileName <- B.getLazyByteStringNul
              mapCycle <- B.getLazyByteStringNul
              B.skip 1
              return SVC_SERVERINFO {..}
            12 -> pure SVC_LIGHTSTYLE
            13 -> pure SVC_UPDATEUSERINFO
            14 -> pure SVC_DELTADESCRIPTION
            15 -> pure SVC_CLIENTDATA
            16 -> pure SVC_STOPSOUND
            17 -> pure SVC_PINGS
            18 -> pure SVC_PARTICLE
            19 -> pure SVC_DAMAGE
            20 -> pure SVC_SPAWNSTATIC
            21 -> pure SVC_EVENT_RELIABLE
            22 -> pure SVC_SPAWNBASELINE
            23 -> pure SVC_TEMPENTITY
            24 -> pure SVC_SETPAUSE
            25 -> pure SVC_SIGNONNUM
            26 -> pure SVC_CENTERPRINT
            27 -> pure SVC_KILLEDMONSTER
            28 -> pure SVC_FOUNDSECRET
            29 -> pure SVC_SPAWNSTATICSOUND
            30 -> pure SVC_INTERMISSION
            31 -> pure SVC_FINALE
            32 -> pure SVC_CDTRACK
            33 -> pure SVC_RESTORE
            34 -> pure SVC_CUTSCENE
            35 -> pure SVC_WEAPONANIM
            36 -> pure SVC_DECALNAME
            37 -> pure SVC_ROOMTYPE
            38 -> pure SVC_ADDANGLE
            39 -> pure SVC_NEWUSERMSG
            40 -> pure SVC_PACKETENTITIES
            41 -> pure SVC_DELTAPACKETENTITIES
            42 -> pure SVC_CHOKE
            43 -> pure SVC_RESOURCELIST
            44 -> pure SVC_NEWMOVEVARS
            45 -> pure SVC_RESOURCEREQUEST
            46 -> pure SVC_CUSTOMIZATION
            47 -> pure SVC_CROSSHAIRANGLE
            48 -> pure SVC_SOUNDFADE
            49 -> pure SVC_FILETXFERFAILED
            50 -> pure SVC_HLTV
            51 -> pure SVC_DIRECTOR
            52 -> pure SVC_VOICEINIT
            53 -> pure SVC_VOICEDATA
            54 -> SVC_SENDEXTRAINFO <$> B.getLazyByteStringNul <*> B.getInt8
            55 -> pure SVC_TIMESCALE
            56 -> pure SVC_RESOURCELOCATION
            57 -> pure SVC_SENDCVARVALUE
            58 -> pure SVC_SENDCVARVALUE2
            _ -> pure SVC_BAD

          traceM ("message: " ++ show message)

          rest <- getNetworkMessagesInPacket' endPosition
          return (message : rest)

showDemo :: IO ()
showDemo = L.readFile "/tmp/demo.dem" >>= print . B.runGet getDemo
