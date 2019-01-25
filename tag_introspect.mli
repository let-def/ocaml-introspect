type 'a fields
val field_count : 'a fields -> int
val field_get : 'a fields -> int -> 'a

type approx = Obj.Tag_descriptor.approx =
  | Any
  | Char
  | Int
  | Constants of string array
  | Polymorphic_variants

type dynobj
val get_approx : dynobj -> approx
val get_obj : dynobj -> Obj.t
val lift : ?approx:approx -> Obj.t -> dynobj

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

val dynobj : dynobj -> dynval
val obj : Obj.t -> dynval
val value : 'a -> dynval
