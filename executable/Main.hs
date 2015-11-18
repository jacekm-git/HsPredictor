module Main where

import Data.IORef
--
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.FLTKHS
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
-- own
import Callbacks
import HsPredictor.Prelude (sHead, (!?))

maxRows :: Int
maxRows = 20
maxCols :: Int
maxCols = 4
tableData :: [[String]]
tableData = [["Legia", "1", "2", "3"], ["Wisla", "2", "3", "4"]]
headerData :: [String]
headerData = ["Team", "Win", "Draw", "Lose"]
currentHeader :: Int -> String
currentHeader col = case headerData !? col of
  Nothing -> ""
  Just x ->  x
currentCell :: [[String]] -> Int -> Int -> String
currentCell  t row col = case cell of
  Nothing -> ""
  Just x -> x
  where cell = do  t !? row >>= flip (!?) col

drawHeader :: Ref Table -> String -> Rectangle -> IO ()
drawHeader table s rectangle = do
  flcPushClip rectangle
  rhc <- getRowHeaderColor table
  flcDrawBox ThinUpBox rectangle rhc
  flcSetColor blackColor
  flcDrawInBox s rectangle alignCenter Nothing Nothing
  flcPopClip
drawData :: Ref Table -> String -> Rectangle -> IO ()
drawData table s rectangle = do
  flcPushClip rectangle
  flcSetColor whiteColor >> flcRectf rectangle
  flcSetColor gray0Color >> flcDrawInBox s rectangle alignCenter Nothing Nothing
  color' <- getColor table
  flcSetColor color' >> flcRect rectangle
  flcPopClip
drawCell :: IORef [[String]] -> Ref Table
         -> TableContext -> TableCoordinate -> Rectangle -> IO ()
drawCell tableData table context (TableCoordinate (Row row) (Column col)) rectangle = do
  tData <- readIORef tableData
  case context of
   ContextStartPage -> flcSetFont helvetica (FontSize 16)
   ContextColHeader -> drawHeader table (currentHeader col) rectangle
   ContextRowHeader -> drawHeader table (show (row + 1)) rectangle
   ContextCell -> drawData table (currentCell tData row col) rectangle
   _ -> return ()
initializeTable :: Ref Table -> IO ()
initializeTable table = do
  begin table
  setRows table maxRows
  setRowHeader table True
  setRowHeightAll table 20
  setRowResize table False
  setCols table maxCols
  setColHeader table True
  setColWidthAll table 80
  setColResize table True
  end table
main :: IO ()
main = do
  window <- doubleWindowNew
              (Size (Width 400) (Height 300))
              Nothing
              (Just "HsPredictor")
  begin window
  tData <- newIORef tableData
  table <- tableCustom
             (Rectangle
               (Position (X 10) (Y 10))
               (Size (Width 390) (Height 200)))
             Nothing
             Nothing
             (drawCell tData)
             defaultCustomWidgetFuncs
             defaultCustomTableFuncs
  initializeTable table
  btnLoadCsv <- buttonNew (toRectangle (10,230,390,40)) (Just "LoadCSV")
  input_ <- inputNew (toRectangle (10,270,390,30)) Nothing Nothing
  setResizable window (Just table)
  setCallback btnLoadCsv (btnCallback tData table input_)
  end window
  showWidget window
  _ <- FL.run
  return ()
