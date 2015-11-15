-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF.Surface
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- A 'Surface' defines how a pgfpicture should be placed and compiled. Surfaces
-- are used for rendering a @.tex@ or @.pdf@ using functions from
-- 'Diagrams.Backend.PGF'.
--
-- Surfaces are also used in 'Diagrams.Backend.PGF.Typeset' for querying
-- envelopes of text.
--
-- Surfaces for LaTeX, ConTeXt and plain TeX are provided and reexported by
-- Diagrams.Backend.PGF, but can be adjusted here as required.
-----------------------------------------------------------------------------

module Diagrams.Backend.PGFSystem.Surface
    ( -- * Surface definition
      Surface(..)
    , TeXFormat(..)
      -- * Predefined surfaces
    , latexSurface
    , contextSurface
    , plaintexSurface
      -- * Lenses
    , texFormat
    , command
    , arguments
    , pageSize
    , preamble
    , beginDoc
    , endDoc
    ) where

import Diagrams.TwoD.Types
import Diagrams.Backend.PGF.Surface hiding (contextSurface,
                                     latexSurface, plaintexSurface)

latexSurface :: Surface
latexSurface = Surface
  { _texFormat = LaTeX
  , _command   = "pdflatex"
  , _arguments = []
  , _pageSize  = Nothing
  , _preamble  = "\\catcode`\\@=11\n\\documentclass{article}\n"
              ++ "\\def\\pgfsysdriver{pgfsys-pdftex.def}\n\\usepackage{pgfsys}"
  , _beginDoc  = "\\begin{document}\n\\hbox{"
  , _endDoc    = "}\n\\end{document}"
  -- , _pdfOrigin = Just (-2.712, -1.85)
  }

contextSurface :: Surface
contextSurface = Surface
  { _texFormat = ConTeXt
  , _command   = "context"
  , _arguments = ["--pipe"]
  , _pageSize  = Just $ \(V2 w h) ->
                 "\\definepapersize[diagram][width="++show w++"px,height="++show h++"px]\n"
              ++ "\\setuppapersize[diagram][diagram]\n"
              ++ "\\setuplayout\n"
              ++ "[ topspace=" ++ show h ++ "px\n"
              ++ ", backspace=0px\n"
              ++ ", header=0px\n"
              ++ ", footer=0px\n"
              ++ ", width=" ++ show w ++ "px\n"
              ++ ", height=" ++ show h ++ "px\n"
              ++ "]\n"
  , _preamble  = "\\catcode`\\@=11\n"
              ++ "\\usemodule[pgfsys]\n"
              ++ "\\setuppagenumbering[location=]"
  , _beginDoc  = "\\starttext"
  , _endDoc    = "\\stoptext"
  }

plaintexSurface :: Surface
plaintexSurface = Surface
  { _texFormat = PlainTeX
  , _command   = "pdftex"
  , _arguments = []
  , _pageSize  = Nothing
  , _preamble  = "\\catcode`\\@=11\n"
              ++ "\\input pgfsys\n"
              ++ "\\def\\frac#1#2{{\\begingroup #1\\endgroup\\over #2}}"
  , _beginDoc  = ""
  , _endDoc    = "\\bye"
  -- , _pdfOrigin = Just (-0.712, 0.02)
  }
