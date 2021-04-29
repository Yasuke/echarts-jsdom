{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ECharts
  ( initECharts
  , setOption
  , setOptionJSVal
  , onRenderedAction
  , onFinishedAction
  , module X
  , ECharts(..)
  , EChartOpts(..)
  )
  where

import ECharts.Types as X
import ECharts.ChartOptions as X
import ECharts.Series as X
import ECharts.Data as X
import ECharts.Internal
import ECharts.Internal.EChartSeries

import Control.Monad (void)
import qualified Data.Text as T
import Language.Javascript.JSaddle ( eval
                                   , call
                                   , function
                                   , fun
                                   , toJSVal
                                   , obj
                                   , (<#)
                                   )
import GHCJS.DOM.Types (Element)
import Data.Some (Some)
import GHCJS.DOM.Types

data ECharts = ECharts { unECharts :: JSVal }

data EChartOpts =
  EChartOpts { _echartsOpts_size :: (Int, Int) -- ^ Width and Height in pixels
             }
  deriving (Eq, Show)

echartOptsToObj :: EChartOpts -> JSM JSVal
echartOptsToObj (EChartOpts (w,h)) = do
  opts <- obj
  _ <- (opts <# ("width" :: JSString)) w
  _ <- (opts <# ("height" :: JSString)) h
  toJSVal opts

initECharts :: GHCJS.DOM.Types.Element -> EChartOpts -> JSM ECharts
initECharts e opts = do
  f <- eval $ T.pack "(function(e, o) { return echarts['init'](e, null, o) })"
  arg <- toJSVal e
  o <- echartOptsToObj opts
  ECharts <$> call f f [arg, o]

setOption :: ECharts -> ChartOptions -> JSM ()
setOption c opts = do
  options <- toJSVal opts
  setOptionJSVal c options

setOptionJSVal :: ECharts -> JSVal -> JSM ()
setOptionJSVal c options = do
  f <- eval $ T.pack "(function(e, opt) { e['setOption'](opt); })"
  let chart = unECharts c
  void $ call f f [chart, options]

onRenderedAction :: ECharts -> (JSM ()) -> JSM ()
onRenderedAction c action = do
  jsF <- toJSVal =<< function (fun $ \_ _ _ -> action)
  f <- eval $ T.pack "(function(e, opt) { e['on']('rendered', opt); })"
  let chart = unECharts c
  void $ call f f [chart, jsF]

onFinishedAction :: ECharts -> (JSM ()) -> JSM ()
onFinishedAction c action = do
  jsF <- toJSVal =<< function (fun $ \_ _ _ -> action)
  f <- eval $ T.pack "(function(e, opt) { e['on']('finished', opt); })"
  let chart = unECharts c
  void $ call f f [chart, jsF]

instance ToJSVal ChartOptions where
  toJSVal o = toJSVal =<< toEChartConfig o

instance ToJSVal (Some SeriesT) where
  toJSVal o = toJSVal =<< toEChartSeries o
