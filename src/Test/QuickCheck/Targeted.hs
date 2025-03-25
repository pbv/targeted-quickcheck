{-
  Targeted generators for QuickCheck, based on the ISSTA'17 paper
  "Targeted Property-Based Testing" by Andreas Löscher and
  Konstantino Sogonas.

  The key idea is to use an objetive function and classical search algorithms
  to guide the generation of inputs for properties.
  This should allow for more through testing of properties when
  the "interesting" cases are sparse over the state space of the inputs.

  This library implements generation using Hill Climbing and Simulated
  Annealing. To use it, simply import Test.QuickCheck.Target
  (alongside the usual QuickCheck imports) and write generators using
  the strategies defined in this module.  See the examples in the test
  suite for guidance.

  Pedro Vasconcelos, 2025.
-}

module Test.QuickCheck.Targeted
 (Objective(..),                                    -- ^ objective functions
  hillClimbing, boundedClimbing, simulatedAnnealing,  -- ^ search strategies
  schedLinear, schedQuadratic,          -- ^ temperature scheduling (for SA)
  ) where

import qualified Test.QuickCheck as QC

------------------------------------------------------------------------
-- types related to optimization concepts
------------------------------------------------------------------------

-- the "utility"  for a generated value (just an integer)
type Utility = Int

-- | an objetive function for values of type `a' 
data Objective a 
  = Maximize (a -> Utility)
  | Minimize (a -> Utility)



-- | Hill Climbing
-- Start with an approximation and iteratively transition to neighbours,
-- improving the utility value monotonically.
-- see e.g. https://en.wikipedia.org/wiki/Hill_climbing
-- This is defined over an arbitrary monad, but normaly it be used on Gen;
-- in that case it can be seen as stochastic Hill climbing.

hillClimbing :: Monad m
             => Objective a      -- ^ objetive function
             -> (a -> m a)       -- ^ neighbour generation function
             -> Int              -- ^ number of neighbours to try 
             -> a                -- ^ initial candidate
             -> m a              -- ^ local optimum
hillClimbing obj next neighbs s0
  = loop 0 s0 (util s0) 
  where
    util = case obj of
             Maximize f -> f
             Minimize f -> negate . f
    loop k s uv
      | k>=neighbs = return s
      | otherwise = do
          s' <- next s
          let uv' = util s'
          if uv'>uv then
            loop 0 s' uv'
            else
            loop (k+1) s uv


-- | Bounded hill climbing
-- Same as above, but stops after a fixed number of iterations;
-- this can be useful to ensure termination and for comparison with
-- other methods
boundedClimbing :: Monad m
                => Objective a      -- ^ objetive function
                -> (a -> m a)       -- ^ generate a neighbour
                -> Int              -- ^ number of iterations to perform
                -> a                -- ^ initial candidate
                -> m a              -- ^ final approximation
boundedClimbing obj next steps s0
  = loop 0 s0 (util s0) 
  where
    util = case obj of
             Maximize f -> f
             Minimize f -> negate . f
    loop k s uv
      | k>=steps = return s
      | otherwise = do
          s' <- next s
          let uv' = util s'
          if uv'>uv then
            loop (k+1) s' uv'
            else
            loop (k+1) s uv

-----------------------------------------------------------------------
-- Simulated Annealing
-- Iterate over neighbours but allow transitions to worse states
-- as a function of a decreasing "temperature" 
-- See e.g. https://en.wikipedia.org/wiki/Simulated_annealing
-----------------------------------------------------------------------
-- This is restricted to the Gen monad because we need to
-- to sample random variable when deciding transitions to worse states

simulatedAnnealing
  ::  Objective a          -- ^ objetive function
  -> (a -> QC.Gen a)       -- ^ generate a random neighbour
  -> (Double -> Double)    -- ^ temperature scheduling function
  -> Int                   -- ^ number of iterations
  -> a                     -- ^ initial approximation
  -> QC.Gen a
simulatedAnnealing obj next schedule steps s0
  = loop 0 s0 (util s0) 
  where
    util = case obj of
             Minimize f -> f
             Maximize f -> negate . f
    loop k s uv 
      | k>=steps = return s
      | otherwise = do
          let t = schedule (fromIntegral k/fromIntegral steps)
          -- ^ compute the temperature for the current step
          s' <- next s
          let uv' = util s'
          if uv'<uv then
            loop (k+1) s' uv' 
            else do 
            rand <- QC.choose (0,1)
            if exp(fromIntegral (uv-uv')/t) >= rand 
              then loop (k+1) s' uv'
              else loop (k+1) s  uv

-- | Temperature scheduling functions for SA
-- defined and decreasing on [0,1] -> [0,1] 
schedLinear :: Double -> Double
schedLinear t = 1-t

schedQuadratic :: Double -> Double
schedQuadratic t = (t-1)*(t-1)

