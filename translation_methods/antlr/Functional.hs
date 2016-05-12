--hole = hole
--foo = let bar (Just 5) b = hole
--          lol = hole
--          bar Nothing b = hole
--       in 6
--      where
--           kek 3 4 = let bar Nothing 5 = hole
--                     in 2
--

foo 5 = 5
foo 6 = 1
bar 3 = 2
