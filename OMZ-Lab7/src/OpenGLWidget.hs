{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module OpenGLWidget where

import           Control.Lens
import           Coordinates
import           Data.Convertible
import           Data.Default
import           Data.Typeable                (cast)
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as V
import           Data.Word                    (Word16)
import           Foreign.Marshal              (withArrayLen)
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.Rendering.OpenGL.GL as GL hiding (color)
import           Linear
import           Linear.OpenGL                (m44GLmatrix)
import           Log                          (cleanLog, writeLog)
import           Monomer
import qualified Monomer.Lens                 as L
import           Monomer.Widgets.Single
import           Shaders
import           Types


data OpenGLCompEvent =
  InitOpenGL
  | OpenGLWidgetInit OpenGLWidgetState
  | AppEvent AppEvent
  | Dispose
  | NoOp
  deriving (Show, Eq)

data OpenGLWidgetState = OpenGLWidgetState {
  _ogsProgram       :: Program,
  _ogsVao           :: VertexArrayObject,
  _ogsVbo           :: BufferObject,
  _ogsTexture3D     :: TextureObject,
  _ogsCoordsVAO     :: VertexArrayObject,
  _ogsCoordsVBO     :: BufferObject,
  _ogsCoordsProgram :: Program
} deriving (Show, Eq)

data OpenGLComp =
  OpenGLComp {
      compGLObjects :: Maybe OpenGLWidgetState,
      compAppState  :: AppModel Word16
      }
  deriving (Eq, Show)

buildUIComp
  :: WidgetEnv OpenGLComp OpenGLCompEvent
  -> OpenGLComp
  -> WidgetNode OpenGLComp OpenGLCompEvent

buildUIComp _wenv model = defaultWidgetNode "OpenGL Widget" (makeOpenGLWidget model)



openGLWidget :: OpenGLComp -> WidgetNode OpenGLComp OpenGLCompEvent
openGLWidget compModel = defaultWidgetNode "OpenGLWidget" (makeOpenGLWidget compModel)

makeOpenGLWidget ::  OpenGLComp -> Widget OpenGLComp OpenGLCompEvent

makeOpenGLWidget compModel@(OpenGLComp glObjects appModel@(AppModel {appImages=images, appRotationXZ=alpha, appY=y})) = widget where
  widget = createSingle compModel def {
              singleInit = initialize,
              singleMerge = merge,
              singleDispose = dispose,
              singleHandleMessage = handleMessage,
              singleGetSizeReq = getSizeReq,
              singleRender = render,
              singleHandleEvent = handleWheel
  }
  
  rotation = rotateXZ (fromIntegral alpha) !*! rotToCoronal 
  dicomTransMat = projectM !*! translateTo1Octet !*! scaleDown !*! rotation
  coordTransMat = projectM !*! scaleDown

  (rows, cols) = ( imgRows (head images), imgColumns (head images))
  concatImages = concatMap imgPixels images
  normalized = map
              (normalizePeak (minimum concatImages) (maximum concatImages) minBound maxBound)
              concatImages


  handleWheel _wenv node _path (WheelScroll _ _ direction) = Just $
        resultReqs node [
          SendMessage widgetId (AppEvent (
            case direction of
              WheelNormal  -> MoveY 0.05
              WheelFlipped -> MoveY (-0.05)
          ))
        ]
   where  widgetId = node ^. L.info . L.widgetId

  handleWheel _wenv node _path (KeyAction _ key KeyPressed)
    | key == keyUp =
      Just $ resultReqs node [SendMessage widgetId (AppEvent (MoveCamera 0.1))]

    | key == keyDown =
      Just $ resultReqs node [SendMessage widgetId (AppEvent (MoveCamera (-0.1)))]

    where  widgetId = node ^. L.info . L.widgetId

  handleWheel _wenv node _path (KeyAction _ key KeyPressed) 
    | key == key1 = Just $ resultReqs node [SendMessage widgetId (AppEvent (ChangeRotDeg 5))]
    | key == key2 = Just $ resultReqs node [SendMessage widgetId (AppEvent (ChangeRotDeg (-5)))]
    where widgetId = node ^. L.info . L.widgetId
  handleWheel _ _ _ _ = Nothing


  initialize _wenv node = resultReqs node reqs where
    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path

    floatSize = sizeOf (undefined :: Float)
    initOpenGL = do
      cleanLog
      -- This needs to run in render thread

      debugOutput $= Enabled
      debugMessageCallback $= Just (writeLog . show)

      glVersion >>= writeLog

      vbo <- genObjectName
      vao <- genObjectName
      vboCoords <- genObjectName
      vaoCoords <- genObjectName

      bindVertexArrayObject $= Just vao

      bindBuffer ArrayBuffer $= Just vbo


      bufferData ArrayBuffer $= (fromIntegral (floatSize * 4 * 6), nullPtr, DynamicDraw)

      bindVertexArrayObject $= Nothing
      bindBuffer ArrayBuffer $= Just vboCoords

      bufferData ArrayBuffer $= (fromIntegral (floatSize * 2), nullPtr, DynamicDraw)

      tex1 <- genObjectName
      activeTexture $= TextureUnit 0
      textureBinding Texture3D $= Just tex1

      -- rowAlignment Unpack $= 1
      textureFilter Texture3D $= ((Linear', Nothing), Linear')
      textureWrapMode Texture3D S $= (Repeated, ClampToEdge)
      textureWrapMode Texture3D T $= (Repeated, ClampToEdge)
      textureWrapMode Texture3D R $= (Repeated, ClampToEdge)

      withArrayLen normalized (\ _ ptr ->
        let pixData = PixelData Red UnsignedShort ptr in
        texImage3D Texture3D NoProxy 0 R16 (TextureSize3D (fromIntegral cols) (fromIntegral rows) (fromIntegral (length images))) 0 pixData
        )

      prog <- createShaderProgram vertShaderPath fragShaderPath
      currentProgram $= Just prog
      setTransMat prog dicomTransMat
      setTexture1 prog 0


      currentProgram $= Nothing

      progCoords <- createShaderProgram vertSingleAxisPath fragSingleColorPath
      textureBinding Texture2D $= Nothing


      return $ OpenGLWidgetInit (OpenGLWidgetState prog vao vbo tex1 vaoCoords vboCoords progCoords)

    reqs = [RunInRenderThread widgetId path initOpenGL]

  merge _wenv node _oldNode oldModel = resultNode newNode where
    newNode = node
           & L.widget .~ makeOpenGLWidget oldModel

  dispose _wenv node = resultReqs node reqs where

    widgetId = node ^. L.info . L.widgetId
    path = node ^. L.info . L.path
    disposeOpenGL = do
      case glObjects of
        Just (OpenGLWidgetState program1 vao vbo tex3d coordsVAO coordsVbo coordsProg)  -> do
          writeLog "Disposing..."
          deleteObjectName tex3d
          deleteObjectNames [vao, coordsVAO]
          deleteObjectNames [vbo, coordsVbo]
          deleteObjectNames [program1,coordsProg]
          writeLog "disposed"
        Nothing -> pure ()

    reqs = [RunInRenderThread widgetId path disposeOpenGL]

  handleMessage _wenv node _target msg = case cast msg of
    Just (OpenGLWidgetInit gls) -> Just result where
      glObj = Just gls
      newModel = OpenGLComp glObj appModel
      newNode = node
        & L.widget .~ makeOpenGLWidget newModel
      result = resultReqs newNode [RenderOnce]
    Just (AppEvent (MoveY tY)) ->
      let OpenGLComp glObj appM@AppModel {appY=y} = compModel
          newY =
            case y + tY of
                n | n < -1 -> 1
                n | n > 1 -> -1
                n         -> n
          newNode = node & L.widget .~ makeOpenGLWidget (OpenGLComp glObj appM {appY = newY})
      in
        
        Just $ resultReqs newNode []

    Just (AppEvent (ChangeRotDeg deltaDeg)) ->
      let OpenGLComp glObj appM@AppModel {appRotationXZ=deg} = compModel
          newNode = node & L.widget .~ makeOpenGLWidget (OpenGLComp glObj appM {appRotationXZ = deg+deltaDeg})
      in
        Just $ resultReqs newNode []
        where widgetId = node ^. L.info . L.widgetId
              path = node ^. L.info . L.path

    _ -> Nothing

  getSizeReq _wenv _node = (sizeReqW_, sizeReqH_) where
    sizeReqW_ = fixedSize (fromIntegral cols * 2)
    sizeReqH_ = fixedSize (fromIntegral rows * 2)

  render wenv node renderer_ =
    case glObjects of
      Just actualState -> do
        createRawTask renderer_ (do

          doInScissor winSize dpr offset activeVp $
            let vertices = V.fromList $
                            concat [ map realToFrac [x, z, u, v] | (V2 x z, V2 u v) <- textureCoords]
                OpenGLWidgetState {_ogsCoordsVBO=vbo, _ogsCoordsProgram=prog, _ogsCoordsVAO=vao} = actualState
            in do


            drawVertices y dicomTransMat rotation actualState vertices
            drawCoordinates coordTransMat (vao,vbo, prog)
          )
      Nothing -> writeLog "Nothing to Render."

    where
      dpr = wenv ^. L.dpr
      winSize = wenv ^. L.windowSize
      activeVp = wenv ^. L.viewport
      style = currentStyle wenv node
      nodeVp = getContentArea node style
      offset = wenv ^. L.offset


doInScissor :: Monomer.Size -> Double -> Point -> Monomer.Rect -> IO () -> IO ()
doInScissor winSize dpr offset vp action = do
  -- OpenGL's Y axis increases from bottom to top
  scissor $= Just (Position (round $ rx+ox) (round $ winH - ry - oy - rh), GL.Size (round rw) (round rh))
  action
  scissor $= Nothing
  where
    winH = winSize ^. L.h * dpr
    Monomer.Point ox oy = mulPoint dpr offset
    Rect rx ry rw rh = mulRect dpr vp

normalizePeak :: (Num a, Real a, Convertible Double a) => a -> a -> a -> a -> a -> a
normalizePeak minP maxP newMin newMax value =
  convert $ (realToFrac newMin :: Double) + realToFrac (value - minP) / realToFrac (maxP - minP) * realToFrac (newMax - newMin)


drawCoordinates :: M44 Float ->  (VertexArrayObject, BufferObject, Program) -> IO ()
drawCoordinates transMat (vao, vbo, prog) = do

  bindVertexArrayObject $= Just vao

  bindBuffer ArrayBuffer $= Just vbo

  -- GL coords
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 1 Float (fromIntegral floatSize) nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- let histogrammed = V.map (normalizePeak (minimum img) (maximum img) minBound maxBound) vertices
  V.unsafeWith vertices $ \vertsPtr ->
    bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral (V.length vertices * floatSize)) vertsPtr

  -- Configure shader program
  currentProgram $= Just prog
  transformMatLoc <- get $ uniformLocation prog "u_transform"
  uniform transformMatLoc $= transMat ^. m44GLmatrix

  unitUniformLoc <- get $ uniformLocation prog unitUniform

  colorLoc <- get $ uniformLocation prog colorUniform

  uniform colorLoc $= red
  uniform unitUniformLoc $= x
  drawArrays Lines 0 2

  uniform colorLoc $= green
  uniform unitUniformLoc $= y
  drawArrays Lines 0 2

  uniform colorLoc $= blue
  uniform unitUniformLoc $= z

  drawArrays Lines 0 2

  where
    red, green, blue :: Vector4 Float
    x,y,z :: Vector3 Float
    red = Vector4 1 0 0 1
    green = Vector4 0 1 0 1
    blue = Vector4 0 0 1 1
    x = Vector3 1 0 0
    y = Vector3 0 1 0
    z = Vector3 0 0 1

    vertices = V.fromList [0, 1] :: Vector Float
    floatSize = sizeOf (undefined :: Float)

drawVertices :: Float -> M44 Float -> M44 Float -> OpenGLWidgetState  -> Vector Float -> IO ()
drawVertices y transmat rotMat state vertices = do

  bindVertexArrayObject $= Just vao

  bindBuffer ArrayBuffer $= Just vbo

  -- let histogrammed = V.map (normalizePeak (minimum img) (maximum img) minBound maxBound) vertices
  V.unsafeWith vertices $ \vertsPtr ->
    bufferSubData ArrayBuffer WriteToBuffer 0 (fromIntegral (V.length vertices * floatSize)) vertsPtr

  -- GL coords
  vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) nullPtr)
  vertexAttribArray (AttribLocation 0) $= Enabled

  -- UV
  vertexAttribPointer (AttribLocation 1) $=
    (ToFloat, VertexArrayDescriptor 2 Float (fromIntegral (floatSize * 4)) (nullPtr `plusPtr` (floatSize * 2)))

  vertexAttribArray (AttribLocation 1) $= Enabled

  -- Configure shader program
  currentProgram $= Just program
  transformMatLoc <- get $ uniformLocation program transMatUniform
  uniform transformMatLoc $= transmat ^. m44GLmatrix

  yUniformLoc <- get $ uniformLocation program yUniform
  uniform yUniformLoc $= y

  rotateUniform <- get $ uniformLocation program rotateUniform
  uniform rotateUniform $= rotMat ^. m44GLmatrix

  activeTexture $= TextureUnit 0
  textureBinding Texture3D $= Just tex3d
  setTexture1 program 0

  drawArrays Triangles 0 6

  where
    floatSize = sizeOf (undefined :: Float)
    OpenGLWidgetState program vao vbo tex3d _ _ _ = state
