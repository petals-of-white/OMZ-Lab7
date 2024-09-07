module Shaders where
import           Control.Lens                 ((^.))
import           Control.Monad                (unless)
import           Graphics.Rendering.OpenGL.GL as GL
import           Linear                       (M44)
import           Linear.OpenGL                (m44GLmatrix)
import           Log                          (writeLog)


tex1Uniform, tex2Uniform, transMatUniform, yUniform, colorUniform, unitUniform :: String

tex1Uniform = "tex1"
tex2Uniform = "tex2"
transMatUniform = "u_transform"
yUniform = "y"
colorUniform = "Color"
unitUniform = "unit"

rotateUniform :: String
rotateUniform = "u_rotate"

vertShaderPath, fragShaderPath, fragSingleColorPath, vertSingleAxisPath :: String

vertShaderPath = "shaders/vert.glsl"
fragShaderPath = "shaders/frag.glsl"
fragSingleColorPath = "shaders/fragSingleColor.glsl"
vertSingleAxisPath = "shaders/vertSingleAxis.glsl"

createShaderProgram :: FilePath -> FilePath -> IO Program
createShaderProgram vertPath fragPath = do
  shaderProgram <- GL.createProgram
  vertShader <- makeShader VertexShader vertPath
  fragShader <- makeShader FragmentShader fragPath

  attachShader shaderProgram vertShader
  attachShader shaderProgram fragShader

  linkProgram shaderProgram
  checkProgramLink shaderProgram

  validateProgram shaderProgram
  checkProgramValidation shaderProgram

  deleteObjectNames [vertShader, fragShader]

  return shaderProgram

setTransMat :: Program -> M44 Float -> IO ()
setTransMat prog transMat =  do
      currentProgram $= Just prog
      transformMatUniformLoc <- get $ uniformLocation prog transMatUniform
      uniform transformMatUniformLoc $= transMat ^. m44GLmatrix
      writeLog "transmat set"

setTexture :: Program -> String -> GLint -> IO ()
setTexture prog texUniformName texUnit = do
  tex1UniLoc <- get $ uniformLocation prog texUniformName
  uniform tex1UniLoc $= texUnit
  writeLog "samplers 2d set"

setTexture1 :: Program -> GLint -> IO ()
setTexture1 prog = setTexture prog tex1Uniform

setTexture2 :: Program -> GLint -> IO ()
setTexture2 prog = setTexture prog tex2Uniform

checkProgramValidation :: Program -> IO ()
checkProgramValidation shaderProgram = do
  validated <- get (validateStatus shaderProgram)

  if not validated then do
    infolog <- programInfoLog shaderProgram
    writeLog infolog
  else
    writeLog "Program validated"

checkProgramLink :: Program -> IO ()
checkProgramLink shaderProgram = do
  linked <- get (linkStatus shaderProgram)
  if not linked then do
    infolog <- programInfoLog shaderProgram
    writeLog infolog
  else
    writeLog "Program linked"

  unless linked $ do
    infolog <- programInfoLog shaderProgram
    writeLog infolog

makeShader :: ShaderType -> FilePath -> IO Shader
makeShader shType shaderFile = do
  shader <- createShader shType
  src <- readFile shaderFile
  shaderSourceBS shader $= packUtf8 src

  compileShader shader
  checkShaderCompile shader

  return shader

checkShaderCompile :: Shader -> IO ()
checkShaderCompile shader = do
  compiled <- get shaderCompiler
  if not compiled then do
    infolog <- shaderInfoLog shader
    writeLog infolog
  else
    writeLog "shader compiled"

