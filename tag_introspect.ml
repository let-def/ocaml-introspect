include Tagprinter.Introspect

let dynobj = self_dynval
let obj x = self_dynval (lift x)
let value x = self_dynval (lift (Obj.repr x))
