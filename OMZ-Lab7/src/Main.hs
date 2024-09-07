{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           App                        (buildUI, handleEventComp)
import           Control.Monad.Trans.Except
import           Data.Binary                as Binary (Binary, decode, encode)
import           Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Lazy       as L
import           Data.DICOM                 as DICOM
import           Data.List                  (isSuffixOf)
import           Data.Text                  as Text (pack)
import           Data.Word                  (Word16, byteSwap16)
import           Monomer
import           OpenGLWidget
import           System.Directory           as Dir
import           System.Environment         as Env
import           System.FilePath
import           Types

class ByteSwappable a where
  byteSwap :: a -> a

instance ByteSwappable Word16 where
  byteSwap = byteSwap16


data DicomBaseInfo = DicomBaseInfo {
  dbiPixelData     :: ByteString,
  dbiRows          :: Word16,
  dbiColumns       :: Word16,
  dbiIntercept     :: Intercept,
  dbiSlope         :: Slope,
  dbiBitsAllocated :: BitsAllocated
} deriving (Eq, Show)


loadDicom :: FilePath -> ExceptT String IO DicomBaseInfo
loadDicom path = do
  dicomObj <- ExceptT $  DICOM.readObjectFromFile path
  let dicomMap = DICOM.toMap dicomObj
      eitherInfo = mapLeft show $ do
        pixBytes <- DICOM.pixelData dicomMap
        rows_ <- DICOM.rows dicomMap
        cols <- DICOM.columns dicomMap
        intercept <- DICOM.rescaleIntercept dicomMap
        slope <- DICOM.rescaleSlope dicomMap
        bitsAlloc <- DICOM.bitsAllocated dicomMap
        return DicomBaseInfo {
          dbiPixelData = pixBytes,
          dbiRows = rows_,
          dbiColumns = cols,
          dbiIntercept = intercept,
          dbiSlope = slope,
          dbiBitsAllocated = bitsAlloc
        }

  ExceptT (return eitherInfo)

loadDicomInteger :: FilePath -> ExceptT String IO DicomBaseInfo
loadDicomInteger path = do
  dicomObj <- ExceptT $  DICOM.readObjectFromFile path
  let dicomMap = DICOM.toMap dicomObj
      eitherInfo = mapLeft show $ do
        pixBytes <- DICOM.pixelData dicomMap
        rows_ <- DICOM.rows dicomMap
        cols <- DICOM.columns dicomMap
        bitsAlloc <- DICOM.bitsAllocated dicomMap
        return DicomBaseInfo {
          dbiPixelData = pixBytes,
          dbiRows = rows_,
          dbiColumns = cols,
          dbiIntercept = 0,
          dbiSlope = 0,
          dbiBitsAllocated = bitsAlloc
        }

  ExceptT (return eitherInfo)

dicomPixelDataToListBE  :: (Binary p, ByteSwappable p) => Word16 -> Word16 -> ByteString -> [p]
dicomPixelDataToListBE rows_ columns_ = map byteSwap  . dicomPixelDataToList rows_ columns_

loadDicoms :: FilePath -> FilePath -> ExceptT String IO (DicomBaseInfo, DicomBaseInfo)
loadDicoms path1 path2 = do
  dicom1 <- loadDicomInteger path1
  dicom2 <- loadDicomInteger path2
  return (dicom1, dicom2)

dicomPixelDataToList :: (Binary p) => Word16 -> Word16 -> ByteString -> [p]
dicomPixelDataToList rows_ columns_ pixelData_ =
  decode (L.append (encode ( (fromIntegral rows_* fromIntegral columns_) :: Int)) (L.fromStrict pixelData_))

main :: IO ()
main = do
  progName <- Env.getProgName
  args <- Env.getArgs

  case args of
    dicomFolder:_ -> do
      dicomPaths <- map (dicomFolder </>) . filter (isSuffixOf ".dcm") <$> Dir.listDirectory dicomFolder
      dicomsRes <- runExceptT $ mapM loadDicomInteger dicomPaths
      case dicomsRes of
        Left err -> error err
        Right dicoms_ ->

          let images = [ SimpleImage pixels (fromIntegral rows_) (fromIntegral cols_) |
                        DicomBaseInfo {dbiPixelData=pixelData_, dbiRows=rows_, dbiColumns=cols_} <- dicoms_,
                        let pixels =  dicomPixelDataToListBE rows_ cols_ pixelData_]
              model =
                OpenGLComp {
                  compGLObjects = Nothing,
                  compAppState = AppModel {
                        appImages = images,
                        appRotationXZ = 0,
                        appPlane = Coronal,
                        appY = 0
                        }
                }

              config = [
                appWindowTitle (pack progName),
                appWindowIcon "./assets/images/icon.png",
                appWindowResizable False,
                appTheme darkTheme,

                appWindowState $ MainWindowNormal (fromIntegral (imgColumns (head images) * 2), imgColumns (head images) * 2),
                appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf"
                ]
            in startApp model App.handleEventComp buildUI config

    _ -> error "Pass folder with DICOMs"
