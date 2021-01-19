{-# LANGUAGE ScopedTypeVariables #-}

module TestSingleAssoc ( singleAssocTests ) where

import           Data.List
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Test.Tasty.Sugar
import           TestUtils

import           Sample1 ( sample1 )


testInpPath = "test/samples"

sugarCube = mkCUBE
              { rootName = "*.c"
              , expectedSuffix = "expected"
              , inputDir = testInpPath
              , associatedNames = [ ("exe", "exe") ]
              , validParams = [ ("arch", Just ["x86", "ppc"])
                              , ("form", Just ["base", "refined"])
                              ]
              }

singleAssocTests :: [TT.TestTree]
singleAssocTests =
  let (sugar1,_s1desc) = findSugarIn sugarCube sample1
  in [

       -- This is simply the number of entries in sample1; if this
       -- fails in means that sample1 has been changed and the other
       -- tests here are likely to need updating.
       testCase "valid sample" $ 58 @=? length sample1

     , sugarTestEq "correct found count" sugarCube sample1 6 length

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
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "global-max-good.ppc.expected"
                    , expParamsMatch = [("arch", Explicit "ppc"),
                                        ("form", Assumed "refined")]
                    , associated = [ ("exe", p "global-max-good.ppc.exe")
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
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.x86.expected"
                    , expParamsMatch = [ ("arch", Explicit "x86")
                                       , ("form", Assumed "refined")]
                    , associated = [ ("exe", p "jumpfar.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "base") ]
                    , associated = [ ("exe", p "jumpfar.ppc.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "jumpfar.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "refined") ]
                    , associated = [ ("exe", p "jumpfar.ppc.exe")
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
       , Sweets { rootMatchName = "looping-around.c"
                , rootBaseName = "looping-around"
                , rootFile = p "looping-around.c"
                , cubeParams = validParams sugarCube
                , expected =
                  [ Expectation
                    { expectedFile = p "looping-around.expected"
                    , expParamsMatch = [ ("arch", Assumed "x86")
                                       , ("form", Assumed "base") ]
                    , associated = [ ("exe", p "looping-around.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping-around.expected"
                    , expParamsMatch = [ ("arch", Assumed "x86")
                                       , ("form", Assumed "refined")]
                    , associated = [ ("exe", p "looping-around.x86.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping-around.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "base") ]
                    , associated = [ ("exe", p "looping-around.ppc.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "looping-around.ppc.expected"
                    , expParamsMatch = [ ("arch" , Explicit "ppc")
                                       , ("form" , Assumed "refined") ]
                    , associated = [ ("exe", p "looping-around.ppc.exe")
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
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "switching.ppc.base-expected"
                    , expParamsMatch = [ ("form", Explicit "base")
                                       , ("arch", Explicit "ppc")
                                       ]
                    , associated = [ ("exe", p "switching.ppc.exe")
                                   ]
                    }
                  , Expectation
                    { expectedFile = p "switching.x86.refined-expected"
                    , expParamsMatch = [ ("form", Explicit "refined")
                                       , ("arch", Explicit "x86")
                                       ]
                    , associated = [ ("exe", p "switching.x86.exe")
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
