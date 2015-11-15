{-# LANGUAGE NoMonomorphismRestriction #-}

module Mandelbrot where

import Diagrams.Prelude hiding (magnitude)
import Diagrams.Backend.PGF
import Diagrams.TwoD.Typeset

make d = renderPDF' "example/Typeset/latex.pdf" (Width 220) latexSurface d
      >> renderPDF' "example/Typeset/tex.pdf" (Width 220) plaintexSurface d
      >> renderPDF' "example/Typeset/context.pdf" (Width 220) contextSurface d

test :: Diagram PGF R2
test = (rect 12 8 <> mkTypeset (typeset "small" & tSize .~ Large))
   === (rect 12 8 <> mkTypeset (typeset "large" & tSize .~ Small))

