-- Manuel Meirino, S6856349

tripleRecurrence :: Integer -> Integer
tripleRecurrence n = h n 1 0 0
  where
    -- h n a b c = a*f(n) + b*f(n+1) + c*f(n+2)
    h 0 a b c = a + 2 * b + 3 * c -- a*f(0) + b*f(1) + c*f(2) --> a*1 + b*2 + c*3
    h n a b c = h (n - 1) c (a + 2 * c) (b + c)

-- dafny style
-- invariant: h n a b c = a*f(n) + b*f(n+1) + c*f(n+2)
-- by definition, f(k) = f(k-1) + 2*f(k-2) + f(k-3)

-- we make n one smaller at a time, so
-- a·f(n) + b·f(n+1) + c·f(n+2)    should become    a'·f(n-1) + b'·f(n) + c'·f(n+1)
-- unknown term is the f(n+2), so call the definition, f(n+2) = f(n+1) + 2*f(n) + f(n-1)
-- so, a*f(n) + b*f(n+1) + c*f(n+2) = a*f(n) + b*(f(n+1)) + c*(f(n+1) + 2*f(n) + f(n-1))
-- a*f(n) + b*f(n+1) + c*f(n+2) = (a + 2*c)*f(n) + (b + c)*f(n+1) + c*f(n-1)