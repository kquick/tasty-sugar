{-# LANGUAGE ScopedTypeVariables #-}

module TestMultiAssoc ( multiAssocTests ) where

import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils

import           Sample1 ( sample1 )


testInpPath :: FilePath
testInpPath = "tests/samples"

sugarCube :: CUBE
sugarCube = mkCUBE
              { rootName = "*.c"
              , expectedSuffix = "expected"
              , inputDir = testInpPath
              , associatedNames = [ ("exe", "exe")
                                  , ("obj", "o")
                                  , ("include", "h")
                                  , ("c++-include", "hh")
                                  , ("plain", "")
                                  ]
              , validParams = [ ("arch", Just ["x86", "ppc"])
                              , ("form", Just ["base", "refined"])
                              ]
              }

multiAssocTests :: [TT.TestTree]
multiAssocTests =
  let (sugar1,_s1desc) = findSugarIn sugarCube sample1
  in [ testCase "valid sample" $ 50 @=? length sample1
     , sugarTestEq "correct found count" sugarCube sample1 5 length
     , testCase "results" $ compareBags "results" sugar1 $
       let p = (testInpPath </>) in
       [
         Sweets { rootMatchName = "global-max-good.c"
                , rootBaseName = "global-max-good"
                , rootFile = p "global-max-good.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [
                    Expectation
                    { expectedFile = p "global-max-good.x86.expected"
                    , expParamsMatch = [("arch", Explicit "x86"),
                                        ("form", Assumed "base")]
                    , associated = [ ("exe", p "global-max-good.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "global-max-good.x86.expected"
                    , expParamsMatch = [("arch", Explicit "x86"),
                                        ("form", Assumed "refined")]
                    , associated = [ ("exe", p "global-max-good.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "global-max-good.ppc.expected"
                    , expParamsMatch = [("arch", Explicit "ppc"),
                                        ("form", Assumed "base")]
                    , associated = [ ("exe", p "global-max-good.ppc.exe")
                                   , ("obj", p "global-max-good.ppc.o")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "global-max-good.ppc.expected"
                    , expParamsMatch = [("arch", Explicit "ppc"),
                                        ("form", Assumed "refined")]
                    , associated = [ ("exe", p "global-max-good.ppc.exe")
                                   , ("obj", p "global-max-good.ppc.o")
                                   ]
                    }
                  ]
                }

       , Sweets { rootMatchName = "jumpfar.c"
                , rootBaseName = "jumpfar"
                , rootFile = p "jumpfar.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "jumpfar.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "base") ]
                    , associated = [ ("exe", p "jumpfar.x86.exe")
                                   , ("include", p "jumpfar.h")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "refined")]
                    , associated = [ ("exe", p "jumpfar.x86.exe")
                                   , ("include", p "jumpfar.h")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "base") ]
                    , associated = [ ("exe", p "jumpfar.ppc.exe")
                                   , ("include", p "jumpfar.h")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "refined") ]
                    , associated = [ ("exe", p "jumpfar.ppc.exe")
                                   , ("include", p "jumpfar.h")
                                   ]
                    }
                  ]
                }
       , Sweets { rootMatchName = "looping.c"
                , rootBaseName = "looping"
                , rootFile = p "looping.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "looping.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "base") ]
                    , associated = [ ("exe", p "looping.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "refined")]
                    , associated = [ ("exe", p "looping.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "base") ]
                    , associated = [ ("exe", p "looping.ppc.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "refined") ]
                    , associated = [ ("exe", p "looping.ppc.exe")
                                   ]
                    }
                  ]
                }
       , Sweets { rootMatchName = "switching.c"
                , rootBaseName = "switching"
                , rootFile = p "switching.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "switching.x86.base-expected"
                    , expParamsMatch = [ ("form", Explicit "base")
                                       , ("arch", Explicit "x86")
                                       ]
                    , associated = [ ("exe", p "switching.x86.exe")
                                   , ("include", p "switching.h")
                                   , ("c++-include", p "switching.hh")
                                   , ("plain", p "switching")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "switching.ppc.base-expected"
                    , expParamsMatch = [ ("form", Explicit "base")
                                       , ("arch", Explicit "ppc")
                                       ]
                    , associated = [ ("exe", p "switching.ppc.exe")
                                   , ("obj", p "switching.ppc.o")
                                   , ("include", p "switching.h")
                                   , ("c++-include", p "switching.hh")
                                   , ("plain", p "switching")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "switching.x86.refined-expected"
                    , expParamsMatch = [ ("form", Explicit "refined")
                                       , ("arch", Explicit "x86")
                                       ]
                    , associated = [ ("exe", p "switching.x86.exe")
                                   , ("obj", p "switching-refined.x86.o")
                                   , ("include", p "switching.h")
                                   , ("c++-include", p "switching.hh")
                                   , ("plain", p "switching")
                                   ]
                    }
            ]
        }
       , Sweets { rootMatchName = "tailrecurse.c"
                , rootBaseName = "tailrecurse"
                , rootFile = p "tailrecurse.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "tailrecurse.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86"),
                                         ("form", Assumed "base") ]
                    , associated = [ ("exe", p "tailrecurse.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "tailrecurse.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "refined") ]
                    , associated = [ ("exe", p "tailrecurse.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "tailrecurse.expected"
                    , expParamsMatch = [ ("arch", Assumed "ppc"),
                                         ("form", Assumed "base") ]
                    , associated = [ ("exe", p "tailrecurse.ppc.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "tailrecurse.expected"
                    , expParamsMatch = [ ("arch", Assumed "ppc")
                                       , ("form", Assumed "refined") ]
                    , associated = [ ("exe", p "tailrecurse.ppc.exe")
                                   ]
                    }
                  ]
                }
       ]
     ]
