-- | Specifies the base tasty-sweet types and common class instance
-- definitions for those types.

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Sugar.Types where

import           Data.Function ( on )
import qualified Data.List as L
import           Data.Maybe ( catMaybes )
import qualified System.FilePath.GlobPattern as FPGP
#if MIN_VERSION_prettyprinter(1,7,0)
import Prettyprinter
#else
import Data.Text.Prettyprint.Doc
#endif

import Prelude hiding ( exp )


-- | This is the type used to specify file suffixes.  The synonym name
-- is primarily used to indicate where this suffix specification is
-- used.
type FileSuffix = String


-- | Specifies the parameters and patterns to use when searching for
-- samples to build tests from.  The 'mkCUBE' function should be used
-- to obtain a 'CUBE' structure initialized with overrideable
-- defaults.
--
-- The primary elements to specify are the 'rootName' and the
-- 'expectedSuffix'.  With these two specifications (and possibly the
-- 'inputDir') the 'Test.Tasty.Sugar' functionality will be similar to
-- a "golden" testing package.
--
-- The 'validParams' is an optional feature that is useful when
-- multiple expected results files are generated from a single
-- 'rootName', differing by the specified parameters.
--
-- The 'associatedNames' is an optional feature that is useful for
-- when there are other files to be associated with a test in addition
-- to the 'rootFile' and the 'expectedFile'.
--
data CUBE = CUBE
   {
     -- | The directory in which the sample files that drive the
     -- testing exist.  When specified as a relative filepath
     -- (suggested) then this directory is relative to the cabal file.
     inputDir :: FilePath

     -- | The name of the "root" file for each test scenario.  The
     -- contents of this file are opaque to 'tasty-sweet' and are
     -- interpreted by the tests themselves.  Each "root" file is
     -- the kernel for a set of test cases.
     --
     -- The root file should not be specified with any path element,
     -- it should exist in the 'inputDir' location and it can be
     -- specified as a glob pattern.
     --
     -- The corresponding expected results files will be identified by
     -- finding files which match a portion of this name with a
     -- "{separator}{expectedSuffix}" appended to it.
     , rootName :: FPGP.GlobPattern

     -- | The expected suffix for a target pattern for running a test.
     -- There may be multiple files specifying expected results for a
     -- test (see the 'validParams' below), but a particular test case
     -- is comprised of a source file along with a corresponding
     -- "expected result" file that is the name of the source file
     -- with the 'expectedSuffix' suffix.  The suffix should not contain
     -- any glob match characters. Note that the suffix is the text
     -- that comes after one of the 'separators' below.
     --
     -- The 'expectedSuffix' *may* start with one of the characters in
     -- 'separators'.  If this occurs, then the suffix will only be
     -- considered if preceeded by that specific separator; otherwise
     -- any of the 'separators' may be used prior to the
     -- 'expectedSuffix'.
     , expectedSuffix :: FileSuffix

     -- | The 'separators' specify the characters which separate the
     -- expected suffix from the rootName, and which also separate
     -- the various parameters (if any, see 'validParams' below).  Any
     -- one of the separators in this list can be used, and a file can
     -- used a mixture of the separators in the filename.
     --
     -- It is also valid to specify no separators, in which case the
     -- 'rootName' and 'expectedSuffix' are directly concatenated.  This
     -- is not a typical usage, however.
     --
     -- The default separators (returned by 'mkCUBE') are ".-" meaning
     -- that extensions (and parameters) can be separated from the
     -- base name by either a period or a dash.
     , separators :: Separators

     -- | The 'associatedNames' specifies other files that are
     -- associated with a particular test configuration.  These files
     -- are optional and not all of them appear, but different
     -- suffixes may be associated here with a general name.  When a
     -- test is being generated, any associatedNames that were found
     -- will be passed to the test generator for use as supplemental
     -- data.
     --
     -- Specified as a list of tuples, where each tuple is the
     -- (arbitrary) name of the associated file type, and the file
     -- type suffix (with no period or other separator).
     , associatedNames :: [ (String, FileSuffix) ]

     -- | The 'validParams' can be used to specify various parameters
     -- that may be present in the filename.
     --
     -- For example, tests might be parameterized by which C compiler
     -- (@"gcc"@ or @"clang"@) was used, which target architecture
     -- (@"x86_64"@ or @"ppc"@ or @"arm"@), and which optimization
     -- level.  The values for these parameters appear in any order in
     -- the filenames of any file (other than the 'rootName')
     -- delineated by any of the separators.  Not all parameter values
     -- are required to appear.
     --
     -- The following are valid examples:
     --
     -- > foo-gcc-ppc-O3.o
     -- > foo-clang.x86_64.o
     -- > foo.O0-clang.o
     --
     -- The sugar matching code will attempt to identify the various
     -- parameter values appearing in the _expected_ filename and
     -- provide that information to the test generation process to
     -- allow the generated test to be customized to the available set
     -- of parameters.
     --
     -- The 'associatedNames' provided to the test generator will be
     -- constrained to those associated names that match the parameter
     -- values explicit in the expected name, and called for each
     -- combination of unspecified parameter values present in
     -- associated names.
     --
     -- There may actually be multiple sets of parameterized files for
     -- each 'rootName' file: the test generator will be called for
     -- each set of parameters.
     --
     -- Each entry in the 'validParams' specifies the name of the
     -- parameter and the set of values; one (and only one) parameter
     -- may have existential values rather than pre-determined values,
     -- as indicated by a Nothing for the parameter value set.  Valid
     -- parameter values are *not* matched with file globbing (they
     -- must be explicit and precise matches) and they cannot be blank
     -- (the lack of a parameter is handled automatically rather than
     -- an explicit blank value).
     , validParams :: [ParameterPattern]
   }
   deriving (Show, Read)


-- | Parameters are specified by their name and a possible list of
-- valid values.  If there is no list of valid values, any value is
-- accepted for that parameter position.  Parameters are listed in the
-- order that they should appear in the filenames to be matched.

type ParameterPattern = (String, Maybe [String])

-- | Separators for the path and suffix specifications.  Any separator
-- is accepted in any position between parameters and prior to the
-- expected suffix. The synonym name is primarily used to indicate
-- where this separators specification is intended to be used.

type Separators = String

-- | Generates the default 'CUBE' configuration; callers should override
-- individual fields as appropriate.  This is the preferred way to initialize a
-- CUBE if defaults are to be used for various fields:
--
--   * inputDirs:      [ "test/samples" ]
--   * inputDir:       "test/samples"
--   * separators:     .-
--   * rootName:       *
--   * expectedSuffix: exp

mkCUBE :: CUBE
mkCUBE = CUBE { inputDir = "test/samples"
              , separators = ".-"
              , rootName = "*"
              , associatedNames = []
              , expectedSuffix = "exp"
              , validParams = []
              }


instance Pretty CUBE where
  pretty cube =
    let assoc = prettyAssocNames $ associatedNames cube
        parms = prettyParamPatterns $ validParams cube
        hdrs = [ "input dir: " <+> pretty (inputDir cube)
               , "rootName: " <+> pretty (rootName cube)
               , "expected: " <+>
                 brackets (pretty $ separators cube) <>
                 pretty (expectedSuffix cube)
               ]
    in "Sugar.CUBE" <> (indent 1 $ vsep $ hdrs <> catMaybes [assoc, parms])


-- | Pretty printing for a set of associated names
prettyAssocNames :: [(String, String)] -> Maybe (Doc ann)
prettyAssocNames = \case
  [] -> Nothing
  nms -> Just $ "associated:" <> (indent 1 $ vsep $ map (pretty . fmap show) nms)

-- | Pretty printing for a list of parameter patterns
prettyParamPatterns :: [ParameterPattern] -> Maybe (Doc ann)
prettyParamPatterns = \case
  [] -> Nothing
  prms -> Just $ "params:" <>
          (let pp (pn,mpv) =
                 pretty pn <+> equals <+>
                 case mpv of
                   Nothing -> "*"
                   Just vl -> hsep $
                              L.intersperse pipe $
                              map pretty vl
            in indent 1 $ vsep $ map pp prms)

-- | Each identified test input set is represented as a 'Sweets'
-- object.. a Specifications With Existing Expected Testing Samples.

data Sweets = Sweets
  { rootBaseName :: String
    -- ^ The base of the root path for matching to expected.  This has no path
    -- elements, no extensions and no parameters.  It can be useful to use to
    -- compare to other fields in the 'expected' Expectation list of this
    -- structure.
  , rootMatchName :: String
    -- ^ Matched root.  This is the name of the matched file, (no path elements)
    -- that matched the rootName in the input CUBE.  This includes any extension
    -- or parameter substitutions.  This is often the best name to use for
    -- displaying this matched item.
  , rootFile :: FilePath
    -- ^ The full actual filepath of the matched root, with all path elements,
    -- extensions, parameters, and suffixes present.  This is most useful to open
    -- or otherwise access the file.
  , cubeParams :: [ParameterPattern] -- ^ parameters for match
  , expected :: [Expectation] -- ^ all expected files and associated
  }
  deriving (Show, Eq)

instance Pretty Sweets where
  pretty inp = "Sweet" <+>
               (align $ vsep $ catMaybes
                 [ Just $ pretty (rootMatchName inp)
                 , Just $ "root:" <+>
                   align (vsep [ pretty (rootBaseName inp)
                               , pretty (rootFile inp)
                               ])
                 , prettyParamPatterns $ cubeParams inp
                 , Just $ vsep $ map pretty $ expected inp
                 ])

-- | The 'Association' specifies the name of the associated file entry
-- and the actual filepath of that associated file.

type Association = (String, FilePath)

-- | The 'NamedParamMatch' specifies the parameter name and the
-- corresponding value for the expected file found.  These can be
-- extracted from the name of the expected file and the set of
-- 'ParameterPattern' entries, but they are presented in an associated
-- list format for easy utilization by the invoked test target.

type NamedParamMatch = (String, ParamMatch)

-- | The 'Expectation' represents a valid test configuration based on
-- the set of provided files.  The 'Expectation' consists of an
-- expected file which matches the 'rootFile' in the containing
-- 'Sweets' data object.  The 'expectedFile' field is the name of the
-- file containing expected output, the 'expParamsMatch' field
-- specifies the 'ParameterPattern' matching values for this expected
-- file, and the 'associated' field provides a list of files
-- associated with this expected file.

data Expectation = Expectation
  { expectedFile :: FilePath  -- ^ file containing Expected results
  , expParamsMatch :: [ NamedParamMatch ] -- ^ set of CUBE parameters
                                          -- matched and the matched
                                          -- values.
  , associated :: [ Association ] -- ^ Associated files found
  }
  deriving Show

-- | Equality comparisons of two 'Expectation' objects ignores the
-- order of the 'expParamsMatch' and 'associated' fields.
instance Eq Expectation where
  e1 == e2 = let bagCmp a b = any (a ==) $ L.permutations b
             in and [ expectedFile e1 == expectedFile e2
                    , (bagCmp `on` expParamsMatch) e1 e2
                    , (bagCmp `on` associated) e1 e2
                    ]

instance Pretty Expectation where
  pretty exp =
    let p = expParamsMatch exp
        pp = if null p
             then Nothing
             else Just $ "Matched Params:" <+> (align $ vsep $ map ppp p)
        ppp (n,v) = pretty n <+> equals <+> pretty v
        a = associated exp
        pa = if null a
             then Nothing
             else Just $ "Associated:" <+> (align $ vsep $ map pretty a)
    in hang 4 $ vsep $ catMaybes
       [ Just $ "Expected: " <+> (align $ pretty (expectedFile exp))
       , pp
       , pa
       ]

-- | Indicates the matching parameter value for this identified
-- expected test.  If the parameter value is explicitly specified in
-- the expected filename, it is an 'Explicit' entry, otherwise it is
-- 'Assumed' (for each of the valid 'ParameterPattern' values) or
-- NotSpecified if there are no known 'ParameterPattern' values.

data ParamMatch =
  -- | This parameter value was explicitly specified in the filename
  -- of the expected file.
  Explicit String

  -- | This parameter value was not specified in the filename of the
  -- expected file, so the value is being synthetically supplied.
  -- This is used for parameters that have known values but none is
  -- present: an 'Expectation' is created for each possible parameter
  -- value, identifying each as 'Assumed'.
  | Assumed String

  -- | This parameter value was not specified in the filename for the
  -- expected file.  In addition, the associated 'ParameterPattern'
  -- specified no defined values (i.e. 'Nothing'), so it is not
  -- possible to identify any actual values.  Instead, the
  -- 'Expectation' generated for this expected file will supply this
  -- 'NotSpecified' for this type of parameter.
  | NotSpecified

  deriving (Show, Eq)

instance Pretty ParamMatch where
  pretty (Explicit s) = pretty s
  pretty (Assumed s)  = brackets $ pretty s
  pretty NotSpecified = "*"


-- | The 'paramMatchVal' function is used to determine if a specific
-- value matches the corresponding 'ParamMatch'
paramMatchVal :: String -> ParamMatch -> Bool
paramMatchVal v (Explicit s) = s == v
paramMatchVal v (Assumed s) = s == v
paramMatchVal _ NotSpecified = True


-- | Predicate test returning true for Explicit param values.
isExplicit :: ParamMatch -> Bool
isExplicit = \case
  Explicit _ -> True
  _ -> False


-- | Extracts explicit value or Nothing
getExplicit :: ParamMatch -> Maybe String
getExplicit (Explicit v) = Just v
getExplicit _            = Nothing


----------------------------------------------------------------------

-- | The 'SweetExplanation' is the data type that contains the
-- description of the 'Test.Tasty.Sugar.findSugar' process and
-- results.
data SweetExplanation =
  SweetExpl { rootPath :: FilePath
            , base :: String
            , expectedNames :: [String]  -- ^ candidates
            , results :: Sweets -- ^ actual results
            }

instance Pretty SweetExplanation where
  pretty expl =
    let nms = expectedNames expl
    in align $ vsep $ catMaybes [
      Just $ fillSep $ punctuate ","
        [ "rootPath" <+> dquotes (pretty $ rootPath expl)
        , "base" <+> dquotes (pretty $ base expl)
        , if null nms
          then "no matches"
          else (pretty $ length nms) <+> "possible matches"
        ]
      , if null nms
        then Nothing
        else Just $ indent 8 $ vsep $ map pretty nms
      , Just $ pretty $ results expl
    ]

------------------------------------------------------------------------
