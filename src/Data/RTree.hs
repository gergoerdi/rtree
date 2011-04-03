module Data.RTree (Point(..), Size(..), Rect(..), RTree, empty, insert, hittest) where

import Control.Monad.Writer
import Data.List (minimumBy, sortBy)
import Data.Ord

data Point = Point !Double !Double deriving (Eq, Show)
data Size = Size !Double !Double deriving (Eq, Show)
data Rect = Rect { rect_orig :: !Point,
                   rect_size :: !Size }
            deriving (Eq, Show)
                     
area (Rect _ (Size w h)) = w * h

newtype RTree a = RTree (Maybe (RTree' a))
  
data RTree' a = RBranch { rtree_bb :: Rect, 
                          rtree_len :: !Int,
                          rtree_children :: [RTree' a] }
              | RLeaf { rtree_bb :: Rect, 
                        rtree_item :: a }
                      
empty :: RTree a                      
empty = RTree Nothing

hittest :: Point -> RTree a -> [(Rect, a)]
hittest _ (RTree Nothing) = []
hittest (Point x0 y0) (RTree (Just r)) = sortBy (comparing $ area . fst) $ execWriter (walk r)
  where 
    walk (RBranch bb _ rs) | hit bb = mapM_ walk rs
                           | otherwise = return ()
    walk (RLeaf bb e) | hit bb = tell [(bb, e)]
                      | otherwise = return ()                                        
    hit (Rect (Point x y) (Size w h)) = x0 `between` (x, x + w) && y0 `between` (y, y + h)
    a `between` (min, max) = min <= a && a <= max

branchingFactor :: Int 
branchingFactor = 4

insert :: Rect -> a -> RTree a -> RTree a
insert bb e (RTree Nothing) = RTree . Just $ RLeaf bb e
insert bb e (RTree (Just r)) = RTree . Just $ walk r
  where 
    leaf = RLeaf bb e
    walk r@(RLeaf bb' _) = RBranch (bb' >< bb) 2 [leaf, r]
    walk (RBranch bb' len rs) | len < branchingFactor = RBranch (bb' >< bb) (succ len) (leaf:rs)
                              | otherwise = RBranch (bb' >< bb) len (snd $ minimumBy (comparing (area . fst)) $ inserts rs)
                                            
    inserts [] = []
    inserts (r:rs) = (rtree_bb r >< bb, walk r:rs):(map (fmap (r:)) $ inserts rs)
    
    Rect (Point x y) (Size w h) >< Rect (Point x' y') (Size w' h') = Rect (Point x'' y'') (Size w'' h'')
      where x'' = min x x'
            w'' = (max (x + w) (x' + w')) - x''
            y'' = min y y'
            h'' = (max (y + h) (y' + h')) - y''                    
