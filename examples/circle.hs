#!/usr/env/stack
-- stack runghc --package reanimate-svg
module Main where

import Graphics.SvgTree
--import Graphics.SvgTree.Constructors
import Graphics.SvgTree.Types
import Graphics.SvgTree.Printer
import Graphics.SvgTree.NamedColors


import Control.Lens ((&), (.~), (?~))
import qualified Data.Text as T
import qualified Data.Map as Map
import Codec.Picture (PixelRGBA8 (..))



mkCircle :: Double -> Tree
mkCircle radius = circleTree $ defaultSvg
  & circleCenter .~ (Num 0, Num 0)
  & circleRadius .~ Num radius

mkBackground :: String -> Tree
mkBackground color = withFillOpacity 1 $  withStrokeWidth 0 $
  withFillColor color $ mkRect screenWidth screenHeight

-- | @mkRect width height@ creates a rectangle with given @with@ and @height@, centered at @(0, 0)@.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/rect>
mkRect :: Double -> Double -> Tree
mkRect width height = translate (-width/2) (-height/2) $ rectangleTree $ defaultSvg
  & rectUpperLeftCorner .~ (Num 0, Num 0)
  & rectWidth ?~ Num width
  & rectHeight ?~ Num height

  -- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill-opacity>
withFillOpacity :: Double -> Tree -> Tree
withFillOpacity opacity = fillOpacity ?~ realToFrac opacity

-- | See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/fill>
withFillColor :: String -> Tree -> Tree
withFillColor color = fillColor .~ pure (mkColor color)

mkColor :: String -> Texture
mkColor name =
  case Map.lookup (T.pack name) svgNamedColors of
    Nothing -> ColorRef (PixelRGBA8 240 248 255 255)
    Just c  -> ColorRef c

-- | Merges multiple images into one.
-- See <https://developer.mozilla.org/en-US/docs/Web/SVG/Element/g>
mkGroup :: [Tree] -> Tree
mkGroup forest = groupTree $ defaultSvg
  & groupChildren .~ forest

withStrokeWidth :: Double -> Tree -> Tree
withStrokeWidth width = strokeWidth .~ pure (Num width)

withStrokeColor :: String -> Tree -> Tree
withStrokeColor color = strokeColor .~ pure (mkColor color)

-- | @translate x y image@ moves the @image@ by @x@ along X-axis and by @y@ along Y-axis.
translate :: Double -> Double -> Tree -> Tree
translate x y = withTransformations [Translate x y]

scaleXY :: Double -> Double -> Tree -> Tree
scaleXY x y = withTransformations [Scale x (Just y)]

-- | Apply list of transformations to given image.
withTransformations :: [Transformation] -> Tree -> Tree
withTransformations transformations t =
  mkGroup [t] & transform ?~ transformations

-- | Helper function for pretty-printing SVG nodes as SVG documents.
renderSvg :: Maybe Number -- ^ The number to use as value of the @width@ attribute of the resulting top-level svg element. If @Nothing@, the width attribute won't be rendered.
          -> Maybe Number -- ^ Similar to previous argument, but for @height@ attribute.
          -> Tree         -- ^ SVG to render
          -> String       -- ^ String representation of SVG XML markup
renderSvg w h t = ppDocument doc
-- renderSvg w h t = ppFastElement (xmlOfDocument doc)
  where
    width = 16
    height = 9
    doc = Document
      { _documentViewBox = Just (-width/2, -height/2, width, height)
      , _documentWidth = w
      , _documentHeight = h
      , _documentElements = [withStrokeWidth 1 $ scaleXY 1 (-1) t]
      , _documentDescription = ""
      , _documentLocation = ""
      , _documentAspectRatio = PreserveAspectRatio False AlignNone Nothing
      }

screenWidth = 16
screenHeight = 9




main :: IO ()
main = putStr $
       renderSvg (Just $ Num 800) (Just $ Num 450) $
       mkGroup [mkBackground "white", withFillColor "white" $ withStrokeWidth 0.1 $ withStrokeColor "black" $ mkCircle 2]
