-- | Tree Fractal.
--	Based on ANUPlot code by Clem Baker-Finch.
--	
import Graphics.Gloss
import Data.List

main =  animate (InWindow "Tree" (500, 650) (20,  20))
		black picture


-- The picture is a tree fractal, graded from brown to green
picture :: Float -> Picture	
picture time
	= Translate 0 (-300)
	$ tree (abs ((mod (round (bestMap 0 1 0 5 time)) 8) - 4)) time (dim $ dim brown)


-- Basic stump shape
stump :: Color -> Picture
stump color 
	= Color color
	$ Polygon [(30,0), (15,300), (-15,300), (-30,0)]


-- Make a tree fractal.
tree 	:: Int 		-- Fractal degree
	-> Float	-- time
	-> Color 	-- Color for the stump
	-> Picture

--tree 0 time color = stump color
--tree 0 time color = Pictures [stump color, Translate 0 (300) . Color blossomColor $ thickCircle (blossomSize/4) (blossomSize/2)]
tree 0 time color = Pictures [stump color, Translate 0 (300) $ theSign]
tree n time color 
 = let	smallTree 
		= Rotate (30*(sin time))
		$ Scale 0.5 0.5
		$ tree (n-1) (- time) (greener color)
   in	Pictures
		[ stump color
		, Translate 0 300 $ smallTree
		, Translate 0 240 $ Rotate 20	 smallTree
		, Translate 0 180 $ Rotate (-20) smallTree
		, Translate 0 120 $ Rotate 40 	 smallTree
		, Translate 0  60 $ Rotate (-40) smallTree ]
		

theSign = Pictures [Color red $ filledCircle (blossomSize/2), Scale 0.5 0.5 . Translate (-150) (-50) . Color white $ Text "STOP"]

filledCircle radius = Polygon $ map (makePoint radius) (angleList 8)
--filledCircle radius = Text $ intercalate " " ["foo", "bar"]
--filledCircle radius = Scale 5 5 $ Polygon [(30, 0), (0, 30), (-15, -15)]

makePoint r angle = (sin(angle)*r, cos(angle)*r)

angleList n = map (bestMap 0.5 (n+0.5) 0 (2*pi)) [1..n]

-- <3 <3 <3
bestMap a b c d v = c + ((v - a)/(b - a)) * (d - c)

-- A starting colour for the stump
brown :: Color
brown =  makeColorI 139 100 35  255


-- Make this color a little greener
greener :: Color -> Color
greener c = mixColors 1 10 green c

blossomColor = mixColors 1 3 red black
blossomSize = 250
