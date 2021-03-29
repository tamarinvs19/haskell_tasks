data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Error = EQ
cmp Error _ = GT
cmp Warning Error = LT
cmp Warning Warning = EQ
cmp Warning Info = GT
cmp Info Info = EQ
cmp Info _ = LT

