open StdLabels

type json_typ =
  | ErrInvalid
  | Raw
  | Null
  | Bool
  | Num
  | Str
  | Arr
  | Obj

type json_subtyp =
  | NoneFalseUint
  | TrueSintNoesc
  | Real

module ReadFlag = struct
  type t =
    | No_read_flag
    | Insitu
    | Stop_when_done
    | Allow_trailing_commas
    | Allow_inf_and_nan
    | Number_as_raw
    | Allow_invalid_unicode
  [@@deriving variants]

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor Variants.to_rank x)
end

module ReadCode = struct
  type t =
    | Success
    | Invalid_param
    | Memory_alloc
    | Empty_content
    | Unexpected_content
    | Unexpected_end
    | Unexpected_char
    | Json_structure
    | Invalid_comment
    | Invalid_number
    | Invalid_string
    | Literal
    | File_open
    | File_read
end

module WriteFlag = struct
  type t =
    | NoWriteFlag
    | Pretty
    | EscapeUnicode
    | EscapeSlashes
    | AllowInfAndNan
    | InfAndNanAsNull
    | AllowInvalidUnicode
  [@@deriving variants]

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor Variants.to_rank x)
end

type _ alc

external alc_init : Bigstringaf.t -> [ `Static ] alc = "ml_yyjson_alc_pool_init"
external alc_dyn_new : unit -> [ `Dyn ] alc = "ml_yyjson_alc_dyn_new"
external alc_dyn_free : [ `Dyn ] alc -> unit = "ml_yyjson_alc_dyn_free"
