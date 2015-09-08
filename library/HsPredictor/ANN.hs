module HsPredictor.ANN where

import           HFANN

fannDef :: [Int]
fannDef = [6, 3, 1]

{-| Train artificial neural network and save trained network to file. -}
trainAndSaveANN :: String -- ^ path to exported file
                -> String -- ^ where save network
                -> Int -- ^ number of epochs
                -> Int -- ^ reports frequency (0 - no reports)
                -> Double -- ^ desired error (stops training whech achieved)
                -> IO ()
trainAndSaveANN expPath savePath epochs reports desiredError =
  withStandardFann fannDef $ \fann -> do
    setActivationFunctionHidden fann activationSigmoidSymmetric
    setActivationFunctionOutput fann activationSigmoidSymmetric
    trainOnFile fann expPath epochs reports desiredError
    saveFann fann savePath

useANN :: String -- ^ path to saved ann
       -> [Double] -- ^ input data
       -> IO [Double]
useANN annPath inp = withSavedFann annPath $ \fann -> runFann fann inp
