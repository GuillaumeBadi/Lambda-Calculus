
module ChurchEncoding where

import Types

true = (Ab (Va "a") (Ab (Va "b") (Va "a")))
false = (Ab (Va "a") (Ab (Va "b") (Va "b")))

vs = [Va "a", Va "b"]

dataset = [ ([true, false], true)
          , ([true, true], true)
          , ([false, false], false)
          , ([false, true], true) ]
