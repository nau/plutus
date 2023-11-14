{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
import PlutusCore.Executable.New.Types

import System.Console.GetOpt as GetOpt
import System.Environment
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Control.Lens
import Control.Monad

-- The state of GetOpt
data State = State
    { _curFileType :: FileType
    , _accOptions :: Options
    }
    deriving stock Show
makeLenses ''State

-- Each successful parsing of an option returns a state-transition function
type StateFun = State -> State

optDescrs :: [OptDescr StateFun]
optDescrs =
 [ -- INPUT
   Option [] ["stdin"]  (NoArg $ addInput StdIn)  "Use stdin" -- TODO: don't allow changing but only setting it

   -- OUTPUT
 , Option [] ["stdout"]  (NoArg $ setOutput StdOut)  "Use stdout"
 , Option ['o'] []  (ReqArg (setOutput . AbsolutePath) "FILE")  "Set output file"
   -- INPUT/OUTPUT types
 , Option ['x'] []  (ReqArg (set curFileType . fromJust .  parseExtension) "EXTENSION")  "Change extension"
   -- FIXME: naming,ann partial for data
 , Option ['n'] []  (ReqArg (set (curFileType . fLang . naming) . read) "NAMING")  "Change naming"
 , Option ['a'] []  (ReqArg (set (curFileType . fLang . ann) . read) "ANNOTATION")  "Change annotation"

 -- MODES
 --, Option [] ["compile"]  (NoArg (set (accOptions . mode) Compile)) "Default mode"
 , Option [] ["run"]  (NoArg (set (accOptions . mode) Run)) "Execute program after compilation"
 , Option [] ["bench"]  (OptArg (set (accOptions . mode) . Bench . fmap read) "SECS") "Run repeatedly up to these number of seconds (default: 10)  and print statistics"
 , Option [] ["debug"]  (OptArg (set (accOptions . mode) . Debug) "DIR") "Debug program after compilation"
 , Option ['h'] ["help"]  (NoArg (set (accOptions . mode) Help)) "Show usage"
 , Option [] ["version"]  (NoArg (set (accOptions . mode) Version)) "Show version"
 , Option [] ["print-universe"]  (NoArg (set (accOptions . mode) PrintUni)) "Print the Default universe & builtins"
 , Option [] ["print-samples"]  (NoArg (set (accOptions . mode) PrintSamples)) "Print some program samples"

 -- VERBOSITY
 , Option ['q'] ["quiet"]  (NoArg (set (accOptions . verbosity) VerboseNone)) "Don't print much"
 , Option ['v'] ["verbose"]  (NoArg (set (accOptions . verbosity) VerboseFull)) "Print a lot"

 -- OTHER
 , Option ['O'] []  (ReqArg (set (accOptions . optimiseLvl) . readOptimiseLvl) "INT") "Set optimization level"
 , Option [] ["budget"]  (ReqArg (set (accOptions . budget) . Just . readBudget) "INT,INT") "Debug program after compilation"
 , Option [] ["pretty"]  (ReqArg (set (accOptions . pretty) . read) "STYLE") "Set pretty style"
 , Option [] ["fno-code"]  (NoArg (set (accOptions . fNoCode) True)) "Only typecheck, don't produce code"
 ]

main :: IO ()
main = do
    args <- getArgs
    let (stateFuns, nonDashes, errMsgs) = GetOpt.getOpt (ReturnInOrder fromNonDash) optDescrs args
    when (not $ null nonDashes) $ fail "this should not happen"
    when (not $ null errMsgs) $ fail $ concat errMsgs

    let opts = view accOptions $ foldMap (Dual . Endo) stateFuns `appDual` initState
    runOpts $ if null stateFuns
              then set mode Help opts
              else opts

    where
      -- Dual Endo so as to apply the options in the expected left to right CLI order
      appDual = appEndo . getDual

      initFileType :: FileType
      initFileType = uplc

      initOptions :: Options
      initOptions = Options
          { _inputs = []
          , _output = empty
          , _mode = Compile
          , _pretty = Classic
          , _fNoCode = False
          , _optimiseLvl = SafeOptimise
          , _budget = empty
          , _verbosity = VerboseDefault
          }

      initState :: State
      initState = State initFileType initOptions


runOpts :: Options -> IO ()
runOpts = \case
    Options {_mode=Help} -> putStr $ GetOpt.usageInfo "" optDescrs
    Options {_mode=Version} -> putStrLn "Version 0"
    Options {_mode=PrintUni} -> putStrLn "UNI TODO"
    Options {_mode=PrintSamples} -> putStrLn "SAMPLES TODO"
    Options {_output=Nothing} -> fail "No output file given. Use --stdout to write output to stdout"
    Options {..} -> do
        _ <- compile _inputs _fNoCode _optimiseLvl _pretty $ fromJust _output
        case _mode of
            Compile -> pure ()
            Run -> run _budget
            Debug mdir -> debug mdir _budget
            Bench msecs -> bench msecs _budget

compile is =
    -- foldl compileProgram
    undefined
run=undefined
debug=undefined
bench=undefined

-- Helpers to construct state functions
---------------------------------------

addInput :: FileName -> StateFun
addInput fn s = over (accOptions . inputs) (File (s^.curFileType) fn :) s

setOutput :: FileName -> StateFun
setOutput fn s = set (accOptions . output) (Just $ File (s^.curFileType) fn) s

-- For options that are not prefixed with dash(es), e.g. plain file/dirs
fromNonDash :: FilePath -> StateFun
fromNonDash = addInput . AbsolutePath



-- READING
--------------------------------------------------

-- FIXME: STUBS
deriving stock instance Read Naming
deriving stock instance Read Ann
deriving stock instance Read Pretty

-- FIXME: ugly partial impl
readBudget :: String -> Budget
readBudget s =
    let (cpu, commamem) = break (== ',') s
    in Budget (read cpu) $ read $ tail commamem

-- FIXME: ugly partial imple, or use some enum
readOptimiseLvl :: String -> OptimiseLvl
readOptimiseLvl s = case read @Int s of
    0 -> NoOptimise
    1 -> SafeOptimise
    2 -> SafeOptimise
    3 -> UnsafeOptimise
    _ -> error "cannot read optimise level"

parseExtension :: String -> Maybe FileType
parseExtension = \case
    "uplc" -> Just uplc
    "plc" -> Just plc
    "pir" -> Just pir
    "uplc.flat" -> Just uplcFlat
    "plc.flat" -> Just plcFlat
    "pir.flat" -> Just pirFlat
    "data" -> Just dataFlat
    "uplc.cbor" -> Just uplcCbor
    "plc.cbor" -> Just plcCbor
    "flat" -> Just uplcFlat
    "cbor" -> Just uplcCbor
    _ -> Nothing
