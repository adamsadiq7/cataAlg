
data Fix f  = In(f (Fix f))
 
cata :: Functor f => (f b -> b)-> Fix f -> b
cata alg (In x) = (alg. fmap(cata alg)) x

data Move' k = Fwd Int k
            | Rt Int k
            | Lt Int k
            | Stop

instance Functor Move' where
  fmap f Stop = Stop
  fmap f (Rt x r ) = Rt x (f r)
  fmap f (Lt x r ) = Rt x (f r)
  fmap f (Fwd x r) = Fwd x (f r)

dist:: Fix(Move')-> Int
dist = cata alg where
  alg :: Move' Int -> Int
  alg Stop = 0
  alg (Rt x r) = r
  alg (Lt x r) = r
  alg (Fwd x r) = x + r

data List k = Empty
            | Con Int k

instance Functor List where
  fmap f (Empty) = Empty
  fmap f (Con a k) = Con a (f k)

eval :: Fix(List) -> [Int]
eval = cata alg where
  alg :: List [Int] -> [Int]
  alg (Empty) = []
  alg (Con a k) = a : k
