type 'a fields = int * (int -> 'a)
let field_count (count, _) = count
let field_get (_, getter) x = getter x

type approx =
  | Any
  | Char
  | Int
  | Constants of string array
  | Polymorphic_variants

type dynobj = approx * Obj.t
let get_approx (approx, _ : dynobj) = approx
let get_obj (_, obj : dynobj) = obj
let lift ?(approx=Any) obj : dynobj = (approx, obj)

type dynval =
  | String of string
  | Float of float
  | Char of char
  | Int_or_constant of int * string list
  | Constant of string list
  | Array of dynobj fields
  | Tuple of { name : string; fields : dynobj fields; }
  | Record of { name : string; fields : (string * dynobj) fields; }
  | Polymorphic_variant of string * dynobj
  | Closure
  | Lazy
  | Abstract
  | Custom
  | Unknown

let double_to_wo_shift = match Sys.word_size with
  | 64 -> 0
  | _  -> 1

let no_approx' (_ : int) (obj : Obj.t) : dynobj = (Any, obj)

let fields_of_block f obj =
  if Obj.tag obj = Obj.double_array_tag then
    (Obj.size obj lsr double_to_wo_shift,
     fun i -> f i (Obj.repr (Obj.double_field obj i)))
  else
    (Obj.size obj, fun i -> f i (Obj.field obj i))

let raw_dynval obj =
  let obj = Obj.repr obj in
  if Obj.is_int obj then
    Int_or_constant (Obj.obj obj, [])
  else
    let tag = Obj.tag obj in
    if tag <= Obj.last_non_constant_constructor_tag then (
      if tag = 0
      then Tuple { name = ""; fields = fields_of_block no_approx' obj }
      else Tuple { name = "Tag#" ^ string_of_int tag;
                   fields = fields_of_block no_approx' obj }
    ) else if tag = Obj.string_tag then
      String (Obj.obj obj)
    else if tag = Obj.double_tag then
      Float (Obj.obj obj)
    else if tag = Obj.double_array_tag then
      Array (fields_of_block no_approx' obj)
    else if tag = Obj.closure_tag then
      Closure
    else if tag = Obj.lazy_tag then
      Lazy
    else if tag = Obj.abstract_tag then
      Abstract
    else if tag = Obj.custom_tag then
      Custom
    else
      Unknown

let dynobj = raw_dynval
let obj x = raw_dynval (lift x)
let value x = raw_dynval (lift (Obj.repr x))
