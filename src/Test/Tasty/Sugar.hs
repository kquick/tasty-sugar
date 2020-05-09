-- | Provides test identification by Search Using Golden Answer
-- References.  This is similar in principle to Tasty.KAT and
-- Tasty.Golden, but with different input selection processes.  The
-- intent is that there are multiple different test scenarios, which
-- may all originate with the same input, and that all scenarios are
-- specified by the presence of an "expected" result file along with
-- optional support files.
--
-- A 'Tasty.Sugar.CUBE' object is provided to the 'findSugar' function
-- which returns an array of 'Tasty.Sugar.Sweets' that describe test
-- configurations.
--
-- The 'sugarOptions' should be added to the tasty Options
-- specification, and the 'sugarIngredients' provides additional
-- ingredients for the sugar testing (e.g. the ability to use
-- --showsearch and see the scan and identification of tests).
--
-- The 'withSugarGroups' function can be used to drive the test
-- invocations and group the Sweets by parameter values.
--
-- Example:
--
-- > import Test.Tasty as T
-- > import Test.Tasty.Options
-- > import Test.Tasty.Sugar
-- >
-- > sugarCube = mkCUBE { ... }
-- >
-- > ingredients = T.includingOptions sugarOptions :
-- >               sugarIngredients sugarCube <>
-- >               T.defaultIngredients
-- >
-- > main =
-- >   do testSweets <- findSugar sugarCube
-- >      T.defaultMainWithIngredients ingredients $ withSugarGroups testSweets
--
-- See the README for more information.


-- For example, if the separators are "-." and the validParams are [ ('arch': Just [ 'ppc', 'x86_64' ], 'size'

module Test.Tasty.Sugar
  ( sugarOptions
  , sugarIngredients
  , findSugar
  , findSugarIn
  , CUBE(..)
  , Separators
  , ParameterPattern
  , mkCUBE
  , Sweets(..)
  , Expectation(..)
  , ParamMatch(..)
  , withSugarGroups
  )
where


import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Function
import           Data.List ( intercalate, sort, sortBy, isPrefixOf, isSuffixOf, uncons )
import           Data.Maybe ( catMaybes )
import           Data.Proxy
import           Data.Semigroup ( (<>) )
import           Data.Tagged
import           Data.Text.Prettyprint.Doc
import           Data.Typeable ( Typeable )
import           Options.Applicative
import           System.Directory ( listDirectory )
import           System.FilePath ( (</>) )
import qualified System.FilePath as FP
import           System.FilePath.GlobPattern ( (~~) )
import qualified System.FilePath.GlobPattern as FPGP
import           Test.Tasty.Ingredients
import           Test.Tasty.Options

import Prelude hiding ( exp )


type FileSuffix = String


-- | Specifies the parameters and patterns to use when searching for
-- samples to build tests from.  The 'mkCUBE' function should be used
-- to obtain a 'CUBE' structure initialized with overrideable
-- defaults.
data CUBE = CUBE
   {
     -- | The directory in which the sample files that drive the
     -- testing exist.  When specified as a relative filepath
     -- (suggested) then this directory is relative to the cabal file.
     inputDir :: FilePath

     -- | The name of the "source" file for each test scenario.  The
     -- contents of this file are opaque to 'tasty-sweet' and are
     -- interpreted by the tests themselves.  Each "source" file is
     -- the kernel for a set of test cases.
     --
     -- The source file should not be specified with any path element,
     -- and it can be specified as a glob pattern.
     --
     -- The corresponding expected results files will be identified by
     -- finding files which match this name with a "*{expectedSuffix}"
     -- appended to it.
     , sourceName :: FPGP.GlobPattern

     -- | The expected suffix for a target pattern for running a test.
     -- There may be multiple files specifying expected results for a
     -- test (see the 'validParams' below), but a particular test case
     -- is comprised of a source file along with a corresponding
     -- "expected result" file that is the name of the source file
     -- with the expectedSuffix suffix.  The suffix should not contain
     -- any glob match characters.
     , expectedSuffix :: String

     -- | The separators specify the characters which separate the
     -- expected suffix from the sourceName, and which also separate
     -- the various parameters (if any, see 'validParams' below).  Any
     -- one of the separators in this list can be used, and a file can
     -- used a mixture of the separators in the filename.
     --
     -- It is also valid to specify no separators, in which case the
     -- sourceName and expectedSuffix are directly concatenated.  This
     -- is not a typical usage, however.
     , separators :: Separators

   , associatedNames :: [ (String, FileSuffix) ] -- ^ name of associated, suffix for associated (no '.')
   , validParams :: [ParameterPattern] -- KWQ: no empty strings or glob chars allowed in Just values?  OR does the matching methodology support this?
   }
   deriving (Show, Read)


-- | Parameters are specified by their name and a possible list of
-- valid values.  If there is no list of valid values, any value is
-- accepted for that parameter position.  Parameters are listed in the
-- order that they should appear in the filenames to be matched.
type ParameterPattern = (String, Maybe [String])

-- | Separators for the path and suffix specifications.  Any separator
-- is accepted in any position between parameters and prior to the
-- expected suffix.
type Separators = String

-- | Generates the default CUBE configuration; callers should override
-- individual fields as appropriate.
mkCUBE :: CUBE
mkCUBE = CUBE { inputDir = "test/samples"
              , separators = ".-"
              , sourceName = "*"
              , associatedNames = []
              , expectedSuffix = "*.expected"
              , validParams = []
              }

                -- KWQ: disallow glob chars in separators: ()*[]!|  (or just the opening of those: ([*  ??)

instance Pretty CUBE where
  pretty cube =
    let assoc = case associatedNames cube of
                  [] -> emptyDoc
                  nms -> pretty "associated:" <> (indent 1 $ vsep $ map pretty nms)
        parms = case validParams cube of
                  [] -> emptyDoc
                  prms -> pretty "params:" <> (indent 1 $ vsep $ map pretty prms)
        hdrs = [ pretty "input dir: " <+> pretty (inputDir cube)
               , pretty "sourceName: " <+> pretty (sourceName cube)
               , pretty "expected: " <+> brackets (pretty $ separators cube) <+> pretty (expectedSuffix cube)
               ]
    in pretty "Sugar.CUBE" <> (indent 1 $ vsep $ hdrs <> [assoc, parms])


-- | Each identified test input set is represented as a Sweets
-- object.. a Specifications With Existing Expected Testing Samples.
-- The 'inputName' field is a printable name describing this input,
-- the 'sourceFile' field indicates the name of the sourcefile and the
-- 'expected' field is a list of matching "expected" files.
data Sweets = Sweets
  { inputName :: String  -- KWQ: sweetName?
  , sourceFile :: FilePath
  , cubeParams :: [ParameterPattern] -- from CUBE.validParams
  , expected :: [Expectation]
  }
  deriving Show

instance Pretty Sweets where
  pretty inp = pretty "Sweet" <> (indent 1 $
               vsep [ pretty (inputName inp) <>
                      pretty ", source:" <+> pretty (sourceFile inp)
                    , indent 4 $ vsep $ map pretty $ expected inp
                    ])

-- | The Association specifies the name of the associated file entry
-- and the actual filepath of that associated file.
type Association = (String, FilePath)

-- | The NamedParamMatch specifies the parameter name and the
-- corresponding value for the expected file found.  These can be
-- extracted from the name of the expected file and the set of
-- ParameterPattern entries, but they are presented in an associated
-- list format for easy utilization by the invoked test target.
type NamedParamMatch = (String, ParamMatch)

-- | The Expectation is an expected file which matches the sourceFile
-- in the containing Sweets data object.  The 'expectedFile' field is
-- the name of the file containing expected output, the
-- 'expParamsMatch' field specifies the ParameterPattern matching
-- values for this expected file, and the 'associated' field provides
-- a list of files associated with this expected file.
data Expectation = Expectation
  { expectedFile :: FilePath
  , expParamsMatch :: [ NamedParamMatch ]
  , associated :: [ Association ]
  }
  deriving Show

instance Pretty Expectation where
  pretty exp =
    let p = expParamsMatch exp
        pp = if null p
             then emptyDoc
             else pretty "Params:" <> (indent 4 $ vsep $ map ppp p)
        ppp (n,v) = pretty n <+> equals <+> pretty v
        a = associated exp
        pa = if null a
             then emptyDoc
             else pretty "Associated:" <> (indent 2 $ vsep $ map pretty a)
    in vsep [ pretty "Expected: " <+> pretty (expectedFile exp), pp, pa, emptyDoc ]

-- | Indicates the matching parameter value for this identified
-- expected test.  If the parameter value is explicitly specified in
-- the expected filename, it is an Explicit entry, otherwise it is
-- Assumed (for each of the valid ParameterPattern values) or
-- NotSpecified if there are no known ParameterPattern values.
data ParamMatch = Explicit String | Assumed String | NotSpecified
  deriving (Show, Eq)

instance Pretty ParamMatch where
  pretty (Explicit s) = pretty s
  pretty (Assumed s)  = brackets $ pretty s
  pretty NotSpecified = pretty "*"

paramMatchVal :: String -> ParamMatch -> Bool
paramMatchVal v (Explicit s) = s == v
paramMatchVal v (Assumed s) = s == v
paramMatchVal _ NotSpecified = True


data ShowSugarSearch =  ShowSugarSearch Bool deriving (Eq, Ord, Typeable)

instance IsOption ShowSugarSearch where
  defaultValue = ShowSugarSearch False
  parseValue = fmap ShowSugarSearch . safeRead
  optionName = pure $ "showsearch"
  optionHelp = pure $ "Show details of the search for the set of\n\
                      \sample-file driven tests that would be\n\
                      \performed based on the search."
  optionCLParser = ShowSugarSearch <$> switch
                      ( long (untag (optionName :: Tagged ShowSugarSearch String))
                      <> help (untag (optionHelp :: Tagged ShowSugarSearch String))
                      )


sugarOptions :: [OptionDescription]
sugarOptions = [ Option (Proxy :: Proxy ShowSugarSearch)
               ]

sugarIngredients :: CUBE -> [Ingredient]
sugarIngredients pat = [ searchResultsSugarReport pat ]


-- | This is a Tasty "Ingredient" (aka test runner) that can be used
-- to display the search process and results for generating the tests.
searchResultsSugarReport :: CUBE -> Ingredient
searchResultsSugarReport pat = TestManager [] $ \opts _tests ->
  if lookupOption opts == ShowSugarSearch True
  then Just $ do searchinfo <- findSugar' pat
                 let (inps, expl) = searchinfo
                 putStrLn $ show expl
                 putStrLn ""
                 putStrLn $ "Final set of tests [" ++ show (length inps) ++ "]:"
                 putStrLn $ show $ vsep $ map pretty inps
                 return True
  else Nothing


-- | Returns a list of the discovered test configurations (Sweets)
-- that should be run.
findSugar :: MonadIO m => CUBE -> m [Sweets]
findSugar cube = fst <$> findSugar' cube

findSugar' :: MonadIO m => CUBE -> m ([Sweets], Doc ann)
findSugar' pat = findSugarIn pat <$> liftIO (listDirectory $ inputDir pat)

findSugarIn :: CUBE -> [FilePath] -> ([Sweets], Doc ann)
findSugarIn pat allFiles =
  let basedir = inputDir pat
      isSrcMatch n = n FPGP.~~ (sourceName pat)
      srcNames = FP.takeFileName <$> (filter isSrcMatch allFiles)
      sres = map (\n -> checkSrc basedir n allFiles n) srcNames
      inps = concat $ fst <$> sres
      explSrch = vsep $ snd <$> sres
      expl = vsep
             [ pretty "Checking for test inputs in:" <+> pretty basedir
             , indent 2 $
               vsep [ pretty "# files in directory =" <+>
                      pretty (length allFiles)
                    , pretty "# sources matching" <+>
                      dquotes (pretty (sourceName pat)) <+> equals <+>
                      pretty (length srcNames)
                    , pretty "parameters = " <+> pretty (validParams pat)
                    , explSrch
                    ]
             ]
  in (inps, expl)
  where

    checkSrc :: FilePath -> FilePath -> [FilePath] -> FilePath -> ([Sweets], Doc ann)
    checkSrc basedir origSrcPath allNames srcPath =
      let (exp, explFind) = findExpectation origSrcPath allNames (separators pat)
                            (validParams pat) srcPath (expectedSuffix pat)
      in if null exp
         then case removeASuffix srcPath of   -- KWQ: should just drop filexts, not others because that will cause spurious matches?  e.g. source.c and source_llvm.c might both match source.x86.expected.
                Nothing -> ([], explFind) -- no more params to remove from src
                Just srcPath' -> (\e -> vsep [explFind, e]) <$>
                                 checkSrc basedir origSrcPath allNames srcPath'
         else (exp, explFind)

    removeASuffix name =
      let aSeparator = flip elem (separators pat)
          notASeparator = not . aSeparator
          sp = reverse $
               dropWhile aSeparator $
               dropWhile notASeparator $
               reverse $ FP.takeFileName name
      in if null sp then Nothing else Just $ FP.replaceFileName name sp

    -- Finds the possible expected files matching the selected
    -- source. There may be none, and there may be multiples (with
    -- different parameter values; only one per parameter value
    -- combination).  Selects the longest (i.e. most specific) valid
    -- match.
    findExpectation :: FilePath -> [FilePath] -> Separators -> [ParameterPattern]
                    -> FilePath -> FPGP.GlobPattern -> ( [Sweets], Doc ann )
    findExpectation srcPath allNames seps params srcMatch expSuffix =
      let fixedSuffix = dropWhile (== '*') expSuffix

          -- Construct a set of glob-matches for the current srcMatch
          -- and suffix for each separator between the two.  The
          -- srcMatch might already end with a separator, or the
          -- fixedSuffix might start with one.
          expGlob s =
            let direct = srcMatch <> "*" <> fixedSuffix
                withSep = srcMatch <> "*" <> (s:[]) <> fixedSuffix
            in case (uncons fixedSuffix, uncons $ reverse srcMatch) of
              (Just (c, r), Just (e, _))
                | c == s && e == s -> Just [ srcMatch <> r, direct ]
                | c == s -> Just [ direct ]
                | e == s -> Just [ direct ]
                | c `elem` seps -> Nothing
              _ -> Just [ withSep ]
          expGlobs = if null seps
                     then [ srcMatch <> "*" <> fixedSuffix ]
                     else join $ catMaybes $ map expGlob seps

          expNames = filter (\n -> n /= srcPath && any (n ~~) expGlobs) allNames
          expMatches = longestMatches srcMatch seps params expSuffix expNames
          result = mkInpSpec srcMatch srcPath expMatches allNames
          explF = vsep [ pretty "srcPath" <+> dquotes (pretty srcPath) <>
                         pretty ", base" <+> dquotes (pretty srcMatch) <>
                         pretty ", expGlobs" <+> (hsep $ map (dquotes . pretty) expGlobs) <>
                         (if null expNames
                           then pretty ": no matches"
                           else pretty ":" <+> (pretty $ length expNames) <+>
                                pretty "possible matches"
                         )
                       , if null expNames then emptyDoc else
                           indent 8 $ vsep $ map pretty expNames
                       , if null expMatches then emptyDoc else
                           indent 2 $ vsep $ pretty "Results:" :
                           map (indent 2 . pretty) result
                       ]
      in (if null expMatches then [] else result, explF)


    -- Given the source, the parameters, the separators, the expected
    -- suffix, and a list of possible matches (based on
    -- source*suffix), iterate through parameter value lists, longest
    -- to shortest, using each possible separator between them to find
    -- actual matches and eliminate non-matches.
    longestMatches :: String -> Separators -> [ParameterPattern]
                   -> FPGP.GlobPattern -> [FilePath]
                   -> [(FilePath, [NamedParamMatch])]
    longestMatches srcMatch seps params expSuffix candidates =
      let expSfx = dropWhile (== '*') expSuffix
          go :: String -> [ParameterPattern] -> [(FilePath, [(String, ParamMatch)])]
          go nm [] = let possibles :: [String]
                         possibles = ((map (\s -> intercalate (s:[]) [ nm, expSfx ]) seps) <>
                                      [srcMatch <> expSfx])
                         matches = concat $ map findMatch possibles
                         findMatch p = filter (== p) candidates
                         mkRes m = (m, [])
                     in mkRes <$> take 1 matches
          go nm (p:ps) = case snd p of
                             Nothing -> undefined
                             Just vs ->
                               let vmatch :: String -> [(FilePath, [NamedParamMatch])]
                                   vmatch v =
                                     let sm = map (\s -> go (nm <> (s:[]) <> v) ps) seps
                                     in concat $ um (Explicit v) <$> take 1 sm
                                   um vm ms = map (\(f,pl) -> (f, (fst p,vm):pl)) ms
                                   noParam = let np = go nm []
                                             in map (flip um np) $ Assumed <$> vs
                               in concat $ (map vmatch vs <> noParam) -- [go nm []])
      in go srcMatch params

    mkExpectation :: FilePath -> [FilePath] -> FilePath -> [NamedParamMatch] -> Expectation
    mkExpectation srcPath allPaths expFile params =
      Expectation { expectedFile = inputDir pat </> expFile
                  , associated = fmap (inputDir pat </>) <$>
                                 findAssociated srcPath allPaths params (associatedNames pat)
                  , expParamsMatch = params
                  }


    -- findAssociated returns all files associated with this test configuration.
    --
    --  1. remove all files not matching the srcPath base
    --
    --  2. for each associated glob pattern:
    --
    --     a. Remove files not matching the glob pattern
    --
    --     b. filter NamedParamMatches for the results
    --
    --     c. the result of this should be 0 or 1 files
    --
    -- Note that the params is a full set of params in the correct order, and none should be NotSpecified.
    findAssociated :: FilePath -> [FilePath] -> [NamedParamMatch] -> [(String, FPGP.GlobPattern)] -> [Association]
    findAssociated srcPath allPaths params assocs =
      let baseSrcPath = fst $ FP.splitExtension srcPath
          seps = separators pat
          matchSrc = filter (isPrefixOf baseSrcPath) allPaths
          getAssoc (nm,end) = let assocMatch = filter (isSuffixOf end) matchSrc
                              in if null assocMatch
                                 then Nothing
                                 else paramMatch nm end assocMatch params (length baseSrcPath)
          paramMatch _ _ [] _ _ = Nothing
          paramMatch nm end possibles [] matchedLen =
            let finalPossible = filter ((==) ('.':end) . drop matchedLen) possibles
            in if null finalPossible
               then Nothing
               else Just (nm, head $ sortBy (compare `on` length) finalPossible)
          paramMatch nm end possibles ((_,pm):ps) matchedLen =
            let remaining = drop matchedLen
                pmV = case pm of
                  Explicit s -> s
                  Assumed s -> s
                  _ -> error "Invalid NamedParamMatch for associated search"
                pmVlen = length pmV
                nextIsSep s = if length s > 1 then head s `elem` seps else False
                thenIsPM  s = isPrefixOf pmV $ drop 1 s
                matchesPM p = let r = remaining p in and [nextIsSep r, thenIsPM r]
            in (paramMatch nm end (filter matchesPM possibles) ps (1 + pmVlen + matchedLen)) <|>
               paramMatch nm end possibles [] matchedLen
      in catMaybes $ map getAssoc assocs

    mkInpSpec :: String -> FilePath -> [(FilePath, [NamedParamMatch])] -> [FilePath] -> [Sweets]
    mkInpSpec srcMatch srcPath matches allNames =
        [ Sweets { inputName = srcMatch
                 , sourceFile = inputDir pat </> srcPath
                 , cubeParams = validParams pat
                 , expected = uncurry (mkExpectation srcPath allNames) <$> matches
                 }
         ] -- KWQ: multiple items?


-- KWQ: n.b. every Sweets contains cubeParams, but only needed once; pass Cube instead?  What if multiple Cubes used?
withSugarGroups :: [Sweets]
                -> (String -> [a] -> a)  -- ^ usually testGroup
                -> (Expectation -> a)    -- ^ expected that a ~ TestTree
                -> [a]
withSugarGroups sweets mkGroup mkLeaf =
  let -- mkSweetTests :: Sweets -> a
      mkSweetTests sweet =
        mkGroup (inputName sweet) $
        mkParams sweet [] (expected sweet) $ cubeParams sweet

      -- mkParams iterates through the declared expected values to
      -- create a group for each actual value per expectation, calling
      -- the user-supplied mkLeaf at the leaf of each path.

      -- mkParams :: Sweets -> [NamedParamMatch] -> [Expectation] -> [ParameterPattern] -> [a]
      -- No parameterization, just call the test
      mkParams _sweet _pmatches exp [] = map mkLeaf exp
      mkParams sweet pmatches exp ((name,vspec):ps) =
        case vspec of
          Nothing -> undefined
          Just vs -> let f v = mkGroup v $ mkParams sweet (pmatches' v) (subExp v) ps
                         pmatches' v = (name, pmatch v) : pmatches
                         pmatch v = maybe undefined id $ lookup name (expParamsMatch $ head $ subExp v)
                         subExp v = expMatching name v exp
                     in f <$> sort vs

      expMatching :: String -> String -> [Expectation] -> [Expectation]
      expMatching p v exp =
        filter (\e -> maybe False (paramMatchVal v) (lookup p (expParamsMatch e))) exp

  in map mkSweetTests $ sortBy (compare `on` inputName) sweets
