{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Diagrams.Backend.PGFSystem.Render
  ( PGFSystem (..)
  , sizeSpec
  , surface
  , Options (..)
  ) where

import Data.ByteString.Builder (Builder)

import Control.Lens  (Lens', lens, (^.))
import Data.Default  (Default (..))
import Data.Foldable (foldMap)
import Data.Hashable (Hashable (..))
import Data.Maybe    (isJust)
import Data.Tree     (Tree (Node))
import Data.Typeable (Typeable)

import Diagrams.Backend.PGF.Hbox          (Hbox)
import Diagrams.Backend.PGFSystem.Surface (Surface)
import Diagrams.Core.Compile              (RNode (RPrim, RStyle), RTree)
import Diagrams.Core.Types                (Annotation)
-- import Diagrams.Prelude                   (AlphaColour, AttributeClass,
--                                            Backend (..),
--                                            Color (toAlphaColour),
--                                            Monoid (mappend, mempty),
--                                            Path, V2 Double, Renderable (..),
--                                            SizeSpec, Style,
--                                            dissolve,
--                                            getAttr, getDashing,
--                                            getLineCap,
--                                            getLineJoin,
--                                            getLineWidth, getOpacity,
--                                            (<$>))
-- import Diagrams.TwoD.Adjust               (adjustDiaSize2D)
import Diagrams.TwoD.Path                 (getFillRule)
import Diagrams.TwoD.Adjust               (adjustDia2D)
import Diagrams.Attributes
import Diagrams.Prelude


import qualified Graphics.Rendering.PGFSystem as P

data PGFSystem = PGFSystem
  deriving (Show, Typeable)

type instance V PGFSystem = V2
type instance N PGFSystem = Double

instance Backend PGFSystem V2 Double where
  data Render  PGFSystem V2 Double = R P.Put
  type Result  PGFSystem V2 Double = Builder
  data Options PGFSystem V2 Double = PGFOptions
      { _surface    :: Surface    -- ^ Surface you want to use.
      , _sizeSpec   :: SizeSpec V2 Double -- ^ The requested size.
      , _standalone :: Bool       -- ^ Should include preamble etc.
      }
  renderRTree _ ops rt =
    P.renderWith (_surface ops) (_standalone ops) bounds r
    where
      (R r)  = toRender rt
      bounds = specToSize 1 (ops^.sizeSpec)

  adjustDia = adjustDia2D sizeSpec

instance Monoid (Render PGFSystem V2 Double) where
  mempty                  = R $ return ()
  (R ra) `mappend` (R rb) = R (ra >> rb)

toRender :: RTree PGFSystem V2 Double Annotation -> Render PGFSystem V2 Double
toRender = fromRTree -- . splitFills
  where
    -- fromRTree (Node (TransparencyGroup x) rs)
    --   = P $ do
    --       let R r = foldMap fromRTree rs
    --       pgf <- r
    --       return $ P.transparencyGroup x pgf
    fromRTree (Node (RPrim p) _) = render PGFSystem p
    fromRTree (Node (RStyle sty) rs)
      = R . P.scope $ do
          let R r = foldMap fromRTree rs
          style sty
          r
          draw sty
    fromRTree (Node _ rs) = foldMap fromRTree rs

instance Default (Options PGFSystem V2 Double) where
  def = PGFOptions
          { _surface    = def
          , _sizeSpec   = absolute
          , _standalone = True
          }

sizeSpec :: Lens' (Options PGFSystem V2 Double) (SizeSpec V2 Double)
sizeSpec = lens getSize setSize
  where getSize (PGFOptions { _sizeSpec = s }) = s
        setSize o s = o { _sizeSpec = s }

surface :: Lens' (Options PGFSystem V2 Double) Surface
surface = lens getsurface setsurface
  where getsurface (PGFOptions { _surface = s }) = s
        setsurface o s = o { _surface = s }

style :: Style V2 Double -> P.Put
style s = do
  -- P.fillColor <*~ getFillTexture
  P.fillRule  <~  getFillRule
  --
  -- P.lineColor <*~ getLineTexture
  P.lineJoin  <~  getLineJoin
  P.lineCap   <~  getLineCap
  P.dash      <~  getDashing
  P.lineWidth <~  getLineWidth
  where
  (<~) :: (AttributeClass a) => (b -> P.Put) -> (a -> b) -> P.Put
  setter <~ getter = maybe (return ()) setter mAttribute
    where mAttribute = (getter <$>) . getAttr $ s
  infixr 2 <~
  --
  (<*~) :: (AttributeClass a, Color c)
        => (AlphaColour Double -> P.Put) -> (a -> c) -> P.Put
  setColor <*~ getColor = (setColor . fade . toAlphaColour) <~ getColor
  --
  fade = dissolve $ maybe 1 getOpacity (getAttr s)

shouldStroke :: Style V2 Double -> Bool
shouldStroke s = maybe True (> P.epsilon) mLineWidth
  where
    mLineWidth = (getLineWidth <$>) . getAttr $ s

shouldFill :: Style V2 Double -> Bool
shouldFill s = isJust mFillColor
  where
    mFillColor = getNumAttr getFillTexture s

getNumAttr :: AttributeClass (a n) => (a n -> t) -> Style V2 n -> Maybe t
getNumAttr f = (f <$>) . getAttr

draw :: Style V2 Double -> P.Put
draw s = case (shouldFill s, shouldStroke s) of
           (True, True)   -> P.fillStroke
           (True, False)  -> P.fill
           (False, True)  -> P.stroke
           (False, False) -> return ()

------------------------------------------------------------------------
-- Renderable instances

-- instance Renderable (Segment Closed V2 Double) PGFSystem where
--   render b = render b . (fromSegments :: [Segment Closed V2 Double] -> Path V2 Double) . (:[])
--
-- instance Renderable (Trail V2 Double) PGFSystem where
--   render b = render b . pathFromTrail
--
instance Renderable (Path V2 Double) PGFSystem where
  render _ = R . P.path
  -- render _ p = P $ do
  --   -- dirty hack, but we avoid needing state and how often to people try to
  --   -- fill lines?
  --   when (any (isLine . unLoc) . op Path $ p) $ P.fillOpacity 0
  --   P.path p

instance Renderable (Hbox Double) PGFSystem where
  render _ = R . P.hbox

------------------------------------------------------------------------
-- Hashable instances

instance Hashable (Options PGFSystem V2 Double) where
  hashWithSalt s (PGFOptions sf sz st)
    = s  `hashWithSalt`
      sf `hashWithSalt`
      sz `hashWithSalt`
      st
