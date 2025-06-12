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
    | Bignum_as_raw
    | Allow_bom
  [@@deriving variants]

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor (1 lsl Variants.to_rank x))
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
    | More
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
    | PrettyTwoSpaces
    | NewlineAtEnd
  [@@deriving variants]

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor (1 lsl Variants.to_rank x))
end
