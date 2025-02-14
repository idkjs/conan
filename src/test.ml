let invalid_arg fmt = Format.kasprintf invalid_arg fmt

type 'a t =
  | True : 'a t
  | False : 'a t
  | Numeric : 'a Integer.t * 'a Comparison.t -> 'a t
  | Float : float Comparison.t -> float t
  | Unicode_string : string Comparison.t -> string t
  | String : string Comparison.t -> string t
  | Length : int Comparison.t -> string t
  | Regex : Re.t Comparison.t -> Re.t t
  | Date : Ptime.Span.t Comparison.t -> Ptime.t t

let serialize : type a. Format.formatter -> a t -> unit =
 fun ppf -> function
  | True -> Format.pp_print_string ppf "Conan.Test.always_true"
  | False -> Format.pp_print_string ppf "Conan.Test.always_false"
  | Numeric (n, comparison) ->
      Format.fprintf ppf "@[<2>Conan.Test.numeric@ %a@ %a@]" Integer.serialize n
        Serialize.(parens (Comparison.serialize (Integer.serializer_of n)))
        comparison
  | Float comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.float@ %a@]"
        Serialize.(parens (Comparison.serialize float))
        comparison
  | Unicode_string comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.str_unicode@ %a@]"
        Serialize.(parens (Comparison.serialize string))
        comparison
  | String comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.string@ %a@]"
        Serialize.(parens (Comparison.serialize string))
        comparison
  | Length comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.length@ %a@]"
        Serialize.(parens (Comparison.serialize int))
        comparison
  | Regex comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.regex@ %a@]"
        Serialize.(parens (Comparison.serialize (parens re)))
        comparison
  | Date comparison ->
      Format.fprintf ppf "@[<2>Conan.Test.date@ %a@]"
        Serialize.(parens (Comparison.serialize (parens ptime_span)))
        comparison

let pf = Format.fprintf

let pp_int ppf = pf ppf "%d"

let pp_float ppf = pf ppf "%f"

let pp_string ppf = pf ppf "%S"

let pp_ptime ppf v =
  match Ptime.Span.to_int_s v with
  | Some v -> pf ppf "%d" v
  | None -> pf ppf "%.0f" (Ptime.Span.to_float_s v)

let pp : type a. Format.formatter -> a t -> unit =
 fun ppf -> function
  | True -> pf ppf "x"
  | False -> pf ppf "#"
  | Numeric (w, v) -> pf ppf "numeric:%a" (Comparison.pp (Integer.pp w)) v
  | Float v -> pf ppf "float:%a" (Comparison.pp pp_float) v
  | Unicode_string v -> pf ppf "unicode:%a" (Comparison.pp pp_string) v
  | String v -> pf ppf "string:%a" (Comparison.pp pp_string) v
  | Regex v -> pf ppf "regex:%a" (Comparison.pp Re.pp) v
  | Length v -> pf ppf "length:%a" (Comparison.pp pp_int) v
  | Date v -> pf ppf "date:%a" (Comparison.pp pp_ptime) v

let always_true = True

let always_false = False

let numeric w c = Numeric (w, c)

let float c = Float c

let str_unicode c = Unicode_string c

let string c = String c

let length c = Length c

let regex c = Regex c

let date c = Date c

let process : type test v. (test, v) Ty.t -> test t -> v -> v option =
 fun ty test a ->
  match (ty, test) with
  | _, True -> Some a
  | _, False -> None
  | Byte _, Numeric (w, c) -> if Comparison.process w a c then Some a else None
  | Short _, Numeric (w, c) -> if Comparison.process w a c then Some a else None
  | Long _, Numeric (w, c) -> if Comparison.process w a c then Some a else None
  | Quad _, Numeric (w, c) -> if Comparison.process w a c then Some a else None
  | Float _, Float c -> if Comparison.process_float a c then Some a else None
  | Double _, Float c -> if Comparison.process_float a c then Some a else None
  | Unicode_string _, (String c | Unicode_string c) ->
      if Comparison.process_string a c then Some a else None
  | Search _, String c -> if Comparison.process_string a c then Some a else None
  | Regex { case_insensitive; _ }, Regex c -> (
      let re = Comparison.value c in
      let re = if case_insensitive then Re.no_case re else re in
      let regexp = Re.compile re in
      let str, pos, len = Ropes.to_string a in
      match Array.to_list (Re.Group.all (Re.exec ~pos ~len regexp str)) with
      | [] -> None
      | a :: _ -> Some (Ropes.of_string a)
      | exception Not_found -> None
      (* TODO: process the comparison. *))
  | Pascal_string, Numeric _ -> .
  | Search _, Numeric _ -> .
  | Default, Numeric _ -> .
  | Clear, Numeric _ -> .
  | _, _ -> invalid_arg "%a %a not implemented" Ty.pp ty pp test
