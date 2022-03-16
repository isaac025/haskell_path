module MIX.Registers where

data Register = RA
              | RX
              | RI1
              | RI2
              | RI3
              | RI4
              | RI5
              | RI6
              | RJ
              deriving (Show, Eq, Ord, Enum)
