-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGFSystem
-- Maintainer  :  c.chalmers@me.com
--
-- A full-featured PGFSystem backend for diagrams producing PGFSystem code
-- suitable for LaTeX, ConTeXt or plain TeX consumtion.
--
-- To invoke the PGFSystem backend, you have a number of options.
--
-- * You can use 'renderPGF' function to render a 'Diagram' to a ".tex" file;
--
-- * You can use the most flexible 'renderDia' function to access the
-- resulting PGFSystem code directly in memory. For this particular backend
-- the 'renderDia' function has the following type:
--
-- > renderDia :: PGFSystem -> Options PGFSystem R2 -> Diagram PGFSystem R2 -> Blaze.Builder
--
-- The @Options PGFSystem R2@ is specified as following:
--
-- > data Options PGFSystem R2 = PGFOptions
-- >     { _template          :: Surface    -- ^ Surface you want to use.
-- >     , _sizeSpec          :: SizeSpec2D -- ^ The requested size.
-- >     , _readable          :: Bool       -- ^ Pretty print output.
-- >     }
--
-- The 'Surface' is simply a pair of a @header@ and a @footer@. You
-- can use a 'defaultSurface', as well as a number of other predefined
-- surfaces (which can be found in 'Diagrams.Backend.PGF.Surface').
--
-- It is also possible (although not necessary) to use 'Lens'es to
-- access fields of all the datastructures defined in this backend.
module Diagrams.Backend.PGFSystem
    ( -- * Rendering token & options
      PGFSystem (..)
    , B
    , Options (..)
      -- ** Options lenses
    , surface, sizeSpec --, readable
    , module Diagrams.Backend.PGFSystem.Surface
    -- , defaultSurface
      -- * Rendering functions
    ) where

import Control.Lens ((^.))
import Control.Monad (when)
import Data.Default
import Diagrams.Prelude     hiding (r2, view, (<.>))
import System.Directory     hiding (readable)
import System.Exit
import System.FilePath
import System.Process
import System.IO

import qualified Data.ByteString.Char8         as B

import Diagrams.Backend.PGFSystem.Render
import Diagrams.Backend.PGFSystem.Surface

type B = PGFSystem

