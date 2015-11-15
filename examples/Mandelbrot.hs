{-# LANGUAGE NoMonomorphismRestriction #-}

module Mandelbrot where

import Diagrams.Prelude hiding (magnitude)
import Diagrams.Backend.PGF
import qualified Diagrams.Backend.PGFSystem as Sys

pgfM d = pgfR "latex.pdf"   latexSurface d
      >> pgfR "plain.pdf"   plaintexSurface d
      >> pgfR "context.pdf" contextSurface d

sysM d = sysR "latex.pdf"   Sys.latexSurface d
      >> sysR "plain.pdf"   Sys.plaintexSurface d
      >> sysR "context.pdf" Sys.contextSurface d

pgfR n = renderPDF'     ("example/pgf/" ++ n) (Width 400)
sysR n = Sys.renderPDF' ("example/system/" ++ n) (Width 300)

test :: Diagram PGF R2
test = rect 12 8 <> text "\\~^[]£"

sqCirle = hbox "hi"
       <> square 3 <> circle 1 # fc orange # lwN 0.02
