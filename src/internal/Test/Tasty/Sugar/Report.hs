{-# LANGUAGE OverloadedStrings #-}

module Test.Tasty.Sugar.Report
  (
    sweetsKVITable
  , sweetsTextTable
  )
  where

import           Data.KVITable
import           Data.KVITable.Render.ASCII ( render
                                            , defaultRenderConfig
                                            , RenderConfig(..) )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (&), (.~) )
import qualified Prettyprinter as PP
import           System.FilePath ( takeFileName )

import           Test.Tasty.Sugar.Types


sweetsKVITable :: [Sweets] -> KVITable FilePath
sweetsKVITable [] = mempty
sweetsKVITable sweets =
  let t = fromList $ concatMap
          (\s ->
              [
                ( ("base", T.pack $ rootBaseName s)
                  : ("rootFile", T.pack $ rootFile s)
                  : [ (T.pack pn, T.pack $ show $ PP.pretty pv)
                    | (pn,pv) <- expParamsMatch e ]
                  <> [ (T.pack an, T.pack $ takeFileName af)
                     | (an,af) <- associated e ]
                , takeFileName (expectedFile e)
                )
              | e <- expected s
              ])
          sweets
  in t & valueColName .~ "Expected File"

sweetsTextTable :: [CUBE] -> [Sweets] -> Text
sweetsTextTable [] _ = "No CUBE provided for report"
sweetsTextTable _ [] = "No Sweets provided for report"
sweetsTextTable c s =
  let cfg = defaultRenderConfig
            { rowGroup = "base"
                         : "rootFile"
                         : (T.pack . fst <$> take 1 (validParams $ head c))
            , rowRepeat = False
            }
  in render cfg $ sweetsKVITable s
