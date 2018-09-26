-- Running an ST computation with shared regions
runSt2  ::
  STRef (forall mine yours. ST (mine #$\cap$# yours) a) -> a

inMine  :: ST mine  a -> ST (mine #$\cap$# yours) a
inYours :: ST yours a -> ST (mine #$\cap$# yours) a

-- Sharing an STRef we own
share  :: STRef mine a -> ST mine (STRef (mine #$\cap$# yours) a)

-- Using an STRef that was shared with us
use    :: STRef (mine #$\cap$# yours) a -> STRef mine a

-- Algebraic lemmas
symm   :: STRef (mine #$\cap$# yours) a -> STRef (yours #$\cap$# mine) a

