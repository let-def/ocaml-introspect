open Introspect

type outcome =
  | Ostring of string
  | Ofloat of float
  | Oint of int
  | Ochar of char
  | Oarray of outcome list
  | Oconstr of string * outcome list
  | Orecord of (string * outcome) list
  | Oellipsis
  | Oother of string

let rec format_outcome ppf = function
  | Ostring x -> Format.fprintf ppf "%S" x
  | Ofloat x -> Format.fprintf ppf "%f" x
  | Ochar x -> Format.fprintf ppf "%C" x
  | Oint x -> Format.fprintf ppf "%d" x
  | Oarray xs ->
      let format_elements ppf xs =
        List.iter (Format.fprintf ppf "%a;" format_outcome) xs
      in
      Format.fprintf ppf "@[<hov>[|%a|]@]" format_elements xs
  | Oconstr (name, []) ->
      Format.fprintf ppf "%s" name
  | Oconstr (name, xs) ->
      let format_elements ppf xs =
        List.iter (Format.fprintf ppf "%a;" format_outcome) xs
      in
      Format.fprintf ppf "%s (@[<hov>%a@])" name format_elements xs
  | Orecord xs ->
      let format_element ppf (k,v) =
        Format.fprintf ppf "%s: %a;" k format_outcome v
      in
      let format_elements ppf xs =
        List.iter (format_element ppf) xs
      in
      Format.fprintf ppf "{@[<hov>%a@]}" format_elements xs
  | Oellipsis ->
      Format.fprintf ppf "..."
  | Oother str ->
      Format.fprintf ppf "%s" str

let rec var_dump_outcome depth width obj =
  let fmt_fields' ~ellipsis f fields =
    let size = field_count fields in
    let list =
      ref (if size > width then [ellipsis] else [])
    in
    for i = min width size - 1 downto 0 do
      list := f (field_get fields i) :: !list
    done;
    !list
  in
  let fmt_fields f fields = fmt_fields' ~ellipsis:Oellipsis f fields in
  match Introspect.value obj with
  | Char c ->
    Ochar c
  | String str ->
    Ostring str
  | Float flt ->
    Ofloat flt
  | Constant lst ->
    begin match lst with
      | x :: _ -> Oconstr (x, [])
      | [] -> assert false
    end
  | Polymorphic_variant (name, obj) ->
    Oconstr ("`" ^ name, [var_dump_outcome (depth - 1) width obj])
  | Int_or_constant (i, csts) ->
    if i >= 0 && i < List.length csts
    then Oconstr (List.nth csts i, [])
    else Oint i
  | Array a ->
    Oarray (fmt_fields (var_dump_outcome (depth - 1) width) a)
  | Tuple {name; fields} ->
    Oconstr (name, fmt_fields (var_dump_outcome (depth - 1) width) fields)
  | Record {name; fields} ->
    let dump_field (name, v) = (name, var_dump_outcome (depth - 1) width v) in
    Oconstr (
      name,
      [Orecord (fmt_fields' ~ellipsis:("...", Oellipsis) dump_field fields)]
    )
  | Closure  -> Oother "<closure>"
  | Lazy     -> Oother "<lazy>"
  | Abstract -> Oother "<abstract>"
  | Custom   -> Oother "<custom>"
  | Unknown  -> Oother "<unknown>"
(*| Block (tag, fields) ->
      Oconstr (
        "Block." ^ string_of_int tag,
        fmt_fields (var_dump_outcome (depth - 1) width) fields
      )*)

let var_dump_outcome ?approx ?(depth=5) ?(width=80) v =
  var_dump_outcome depth width (lift ?approx (Obj.repr v))

let var_dump v =
  format_outcome Format.std_formatter (var_dump_outcome v);
  Format.pp_print_flush Format.std_formatter ()
