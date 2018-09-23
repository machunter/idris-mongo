public export
CallTrace : Type
CallTrace = List String

myFunc : (a:CallTrace) -> (b:CallTrace) -> CallTrace
myFunc a b = a ++ b
