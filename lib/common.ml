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

  let to_int = function
    | No_read_flag -> 0
    | Insitu -> 1 lsl 0
    | Stop_when_done -> 1 lsl 1
    | Allow_trailing_commas -> 1 lsl 2
    | Allow_inf_and_nan -> 1 lsl 3
    | Number_as_raw -> 1 lsl 4
    | Allow_invalid_unicode -> 1 lsl 5
    | Bignum_as_raw -> 1 lsl 6
    | Allow_bom -> 1 lsl 7
  ;;

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor to_int x)
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

  let to_int = function
    | NoWriteFlag -> 0
    | Pretty -> 1 lsl 0
    | EscapeUnicode -> 1 lsl 1
    | EscapeSlashes -> 1 lsl 2
    | AllowInfAndNan -> 1 lsl 3
    | InfAndNanAsNull -> 1 lsl 4
    | AllowInvalidUnicode -> 1 lsl 5
    | PrettyTwoSpaces -> 1 lsl 6
    | NewlineAtEnd -> 1 lsl 7
  ;;

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor to_int x)
end
