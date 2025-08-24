import Data.Array
-- As we have seen, lazy evaluation makes it hard to reason about when things will be evaluated; 
-- hence including side effects in a lazy language would be extremely unintuitive. Historically, 
-- this is the reason Haskell is pure: initially, the designers of Haskell wanted to make a lazy functional language, 
-- and quickly realized it would be impossible unless it also disallowed side effects.


f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

-- f1 and f2 both use their argument. But there is still a big difference between them. Although f1 uses its argument m, it does not need to know anything about it. m can remain completely unevaluated, and the unevaluated expression is simply put in a list. 
-- Put another way, the result of f1 e does not depend on the shape of e.
-- f2, on the other hand, needs to know something about its argument in order to proceed: was it constructed with Nothing or Just? 
-- That is, in order to evaluate f2 e, we must first evaluate e, because the result of f2 depends on the shape of e.
-- The other important thing to note is that thunks are evaluated only enough to allow a pattern match to proceed, and no further! For example, suppose we wanted to evaluate f2 (safeHead [3^500, 49]). f2 would force evaluation of the call to safeHead [3^500, 49], which would evaluate to Just (3^500)—note that the 3^500 is not evaluated, since safeHead does not need to



knapsack01 :: [Double]   -- values 
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best) 
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w  = m!(i-1, w)
                          | otherwise = max (m!(i-1, w)) 
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20