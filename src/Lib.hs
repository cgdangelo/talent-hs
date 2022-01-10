{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib (showDemo) where

import Control.Exception (throw)
import Control.Monad (replicateM)
import Data.Binary
import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as L
import Data.Word
import GHC.Int (Int16, Int32, Int8)

data Point a = Point a a a

instance Show a => Show (Point a) where
  show (Point x y z) =
    "Point (" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

pointBy :: Num a => B.Get a -> B.Get (Point a)
pointBy f = Point <$> f <*> f <*> f

pointFloat = pointBy B.getFloatle

data Demo = Demo
  { header :: DemoHeader,
    directory :: Directory
  }
  deriving (Show)

data DemoHeader = DemoHeader
  { magic :: L.ByteString,
    demoProtocol :: Int32,
    networkProtocol :: Int32,
    mapName :: L.ByteString,
    gameDirectory :: L.ByteString,
    mapChecksum :: Int32
  }
  deriving (Show)

type Directory = [DirectoryEntry]

data DirectoryEntry = DirectoryEntry
  { entryType :: DirectoryEntryType,
    description :: L.ByteString,
    flags :: Int32,
    cdTrack :: Int32,
    trackTime :: Float,
    frameCount :: Int32,
    offset :: Int32,
    fileLength :: Int32,
    frames :: [Frame]
  }
  deriving (Show)

data DirectoryEntryType
  = Loading
  | Playback
  | UnknownDirectoryType
  deriving (Show)

data Frame
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
        messagesLength :: Int32
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

getDemo :: B.Get Demo
getDemo = do
  header <- getDemoHeader
  directory <- getDirectory
  return Demo {..}

getDemoHeader :: B.Get DemoHeader
getDemoHeader = do
  magic <- B.getLazyByteString 8
  demoProtocol <- B.getInt32le
  networkProtocol <- B.getInt32le
  mapName <- B.getLazyByteString 260
  gameDirectory <- B.getLazyByteString 260
  mapChecksum <- B.getInt32le
  return DemoHeader {..}

getDirectory :: B.Get Directory
getDirectory = do
  directoryOffset <- B.getInt32le
  currentPosition <- B.bytesRead
  B.skip $ fromIntegral $ directoryOffset - fromIntegral currentPosition
  totalEntries <- B.getInt32le
  getDirectoryEntries $ fromIntegral totalEntries

getDirectoryEntries :: Int -> B.Get [DirectoryEntry]
getDirectoryEntries totalEntries = replicateM totalEntries getDirectoryEntry

getDirectoryEntry :: B.Get DirectoryEntry
getDirectoryEntry = do
  entryType <- getDirectoryEntryType
  description <- B.getLazyByteString 64
  flags <- B.getInt32le
  cdTrack <- B.getInt32le
  trackTime <- B.getFloatle
  frameCount <- B.getInt32le
  offset <- B.getInt32le
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
      case frame of
        NextSection -> pure []
        _ -> do
          frames <- getFrames
          return (frame : frames)

getFrame :: B.Get Frame
getFrame = do
  frameType <- B.getInt32le
  case frameType of
    2 -> pure DemoStart
    3 -> getConsoleCommand
    4 -> getClientData
    5 -> pure NextSection
    6 -> getEvent
    7 -> getWeaponAnimation
    8 -> getSound
    9 -> getDemoBuffer
    _ -> pure UnknownFrameType

getConsoleCommand = ConsoleCommand <$> B.getLazyByteString 64

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
  B.skip $ fromIntegral messagesLength
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
  viewAngles <- pointFloat
  forwardMove <- B.getFloatle
  sideMove <- B.getFloatle
  upMove <- B.getFloatle
  lightLevel <- B.getInt8
  buttons <- B.getInt16le
  impulse <- B.getInt8
  weaponSelect <- B.getInt8
  impactIndex <- B.getInt32le
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
  skyName <- B.getLazyByteString 32
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

showDemo :: IO ()
showDemo = L.readFile "/tmp/demo.dem" >>= print . B.runGet getDemo
