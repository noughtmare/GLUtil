{-# LANGUAGE CPP, DefaultSignatures, FlexibleInstances, FlexibleContexts,
             ScopedTypeVariables #-}
-- |Support for writing "Linear" types to uniform locations in
-- shader programs.
module Graphics.GLUtil.Linear (AsUniform(..)) where
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import Graphics.Rendering.OpenGL
import Graphics.GL.Core31
import Linear
import Unsafe.Coerce (unsafeCoerce)

-- | A type class for things we can write to uniform locations in
-- shader programs. We can provide instances of this class for types
-- from "Linear" without introducing orphan instances.
class AsUniform t where
  asUniform :: t -> UniformLocation -> IO ()
  default asUniform :: Uniform t => t -> UniformLocation -> IO ()
  asUniform x loc = uniform loc $= x

getUL :: UniformLocation -> GLint
getUL = unsafeCoerce

castVecComponent :: Ptr (t a) -> Ptr a
castVecComponent = castPtr

castMatComponent :: Ptr (t (f a)) -> Ptr a
castMatComponent = castPtr

mkAsUniformScalar :: Storable a => (GLint -> GLsizei -> Ptr a -> IO b) -> a -> UniformLocation -> IO b
mkAsUniformScalar f x loc = with x $ f (getUL loc) 1

instance AsUniform GLint   where asUniform = mkAsUniformScalar glUniform1iv
instance AsUniform GLuint  where asUniform = mkAsUniformScalar glUniform1uiv
instance AsUniform GLfloat where asUniform = mkAsUniformScalar glUniform1fv

instance AsUniform TextureUnit
instance UniformComponent a => AsUniform (Index1 a)
instance UniformComponent a => AsUniform (Color4 a)
instance UniformComponent a => AsUniform (Color3 a)
instance UniformComponent a => AsUniform (FogCoord1 a)
instance UniformComponent a => AsUniform (Normal3 a)
instance UniformComponent a => AsUniform (TexCoord4 a)
instance UniformComponent a => AsUniform (TexCoord3 a)
instance UniformComponent a => AsUniform (TexCoord2 a)
instance UniformComponent a => AsUniform (TexCoord1 a)
instance UniformComponent a => AsUniform (Vertex4 a)
instance UniformComponent a => AsUniform (Vertex3 a)
instance UniformComponent a => AsUniform (Vertex2 a)

mkAsUniformVec :: Storable (t a) => (GLint -> GLsizei -> Ptr a -> IO b) -> t a -> UniformLocation -> IO b
mkAsUniformVec f v loc = with v $ f (getUL loc) 1 . castVecComponent

instance AsUniform (V1 GLint  ) where asUniform = mkAsUniformVec glUniform1iv
instance AsUniform (V1 GLuint ) where asUniform = mkAsUniformVec glUniform1uiv
instance AsUniform (V1 GLfloat) where asUniform = mkAsUniformVec glUniform1fv

instance AsUniform (V2 GLint  ) where asUniform = mkAsUniformVec glUniform2iv
instance AsUniform (V2 GLuint ) where asUniform = mkAsUniformVec glUniform2uiv
instance AsUniform (V2 GLfloat) where asUniform = mkAsUniformVec glUniform2fv

instance AsUniform (V3 GLint  ) where asUniform = mkAsUniformVec glUniform3iv
instance AsUniform (V3 GLuint ) where asUniform = mkAsUniformVec glUniform3uiv
instance AsUniform (V3 GLfloat) where asUniform = mkAsUniformVec glUniform3fv

instance AsUniform (V4 GLint  ) where asUniform = mkAsUniformVec glUniform4iv
instance AsUniform (V4 GLuint ) where asUniform = mkAsUniformVec glUniform4uiv
instance AsUniform (V4 GLfloat) where asUniform = mkAsUniformVec glUniform4fv

mkAsUniformMat :: Storable (t (f a)) => (GLint -> GLsizei -> GLboolean -> Ptr a -> IO b) -> t (f a) -> UniformLocation -> IO b
mkAsUniformMat f m loc = with m $ f (getUL loc) 1 1 . castMatComponent

instance AsUniform (M22 GLfloat) where asUniform = mkAsUniformMat glUniformMatrix2fv
instance AsUniform (M33 GLfloat) where asUniform = mkAsUniformMat glUniformMatrix3fv
instance AsUniform (M44 GLfloat) where asUniform = mkAsUniformMat glUniformMatrix4fv

-- Support lists of vectors as uniform arrays of vectors.

mkAsUniformList :: Storable (t a) => (GLint -> GLsizei -> Ptr a -> IO b) -> [t a] -> UniformLocation -> IO b
mkAsUniformList f l loc = withArray l $ f (getUL loc) (fromIntegral $ length l) . castVecComponent

instance AsUniform [V1 GLint  ] where asUniform = mkAsUniformList glUniform1iv
instance AsUniform [V1 GLuint ] where asUniform = mkAsUniformList glUniform1uiv
instance AsUniform [V1 GLfloat] where asUniform = mkAsUniformList glUniform1fv

instance AsUniform [V2 GLint  ] where asUniform = mkAsUniformList glUniform2iv
instance AsUniform [V2 GLuint ] where asUniform = mkAsUniformList glUniform2uiv
instance AsUniform [V2 GLfloat] where asUniform = mkAsUniformList glUniform2fv

instance AsUniform [V3 GLint  ] where asUniform = mkAsUniformList glUniform3iv
instance AsUniform [V3 GLuint ] where asUniform = mkAsUniformList glUniform3uiv
instance AsUniform [V3 GLfloat] where asUniform = mkAsUniformList glUniform3fv

instance AsUniform [V4 GLint  ] where asUniform = mkAsUniformList glUniform4iv
instance AsUniform [V4 GLuint ] where asUniform = mkAsUniformList glUniform4uiv
instance AsUniform [V4 GLfloat] where asUniform = mkAsUniformList glUniform4fv

