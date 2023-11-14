{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module PlutusCore.Executable.New.Types where

import Control.Lens

data Naming = Name
            | DeBruijn
            | NamedDeBruijn
            deriving stock (Show)

data Ann = Unit
         | SrcSpan
         | Coverage
         deriving stock (Show)

data Lang = Pir { _naming :: Naming, _ann :: Ann }
          | Plc { _naming :: Naming, _ann :: Ann }
          | Uplc { _naming :: Naming, _ann :: Ann }
          | Data -- FIXME: naming,ann partial for data
          deriving stock (Show)
makeLenses ''Lang

data Format = Text
            | Flat
            | Cbor
            | Json
            deriving stock (Show)

data FileType = FileType
    { _fFormat :: Format
    , _fLang :: Lang
    }
    deriving stock (Show)
makeLenses ''FileType

data FileName = AbsolutePath FilePath
              | StdIn
              | StdOut
              deriving stock (Show)

data File = File
    { fType :: FileType
    , fName :: FileName
    }
    deriving stock (Show)

data OptimiseLvl = NoOptimise
                 | SafeOptimise
                 | UnsafeOptimise
                 deriving stock (Show)

data Mode = Compile
          | Run
          | Bench (Maybe Int)
          | Debug (Maybe FilePath) -- ^ the tx dir
          | PrintUni
          | PrintSamples
          | Help
          | Version
          deriving stock (Show)

data Pretty = None
            | Classic
            | ClassicDebug
            | Readable
            | ReadableDebug
            deriving stock (Show)

data Verbosity = VerboseNone
               | VerboseDefault
               | VerboseFull
               deriving stock (Show)

type MemBudget = Int
type CpuBudget = Int
data Budget = Budget CpuBudget MemBudget
            deriving stock (Show)

-- MAYBE: use barbies to set defaults of Maybe
data Options = Options
    { _inputs :: [File]
    , _output :: Maybe File
    , _mode :: Mode
    , _budget :: Maybe Budget
    , _pretty :: Pretty
    , _fNoCode :: Bool
    , _optimiseLvl :: OptimiseLvl
    , _verbosity :: Verbosity
    }
    deriving stock Show
makeLenses ''Options


-- "smart" extensions
-- MAYBE: I could also make them patterns
uplc, plc, pir, uplcFlat, plcFlat, pirFlat, dataFlat, uplcCbor, plcCbor :: FileType
uplc = FileType Text $ Uplc Name Unit
plc = FileType Text $ Plc Name Unit
pir = FileType Text $ Pir Name Unit
uplcFlat = FileType Flat $ Uplc Name Unit
plcFlat = FileType Flat $ Plc Name Unit
pirFlat = FileType Flat $ Pir Name Unit
dataFlat = FileType Flat Data
uplcCbor = FileType Cbor $ Uplc DeBruijn Unit
plcCbor = FileType Cbor $ Plc DeBruijn Unit
-- pirCbor = FileType Cbor $ Pir DeBruijn Unit -- not available
