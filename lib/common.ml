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
  (* Bit positions must match YYJSON_READ_* in yyjson.h. The values below
     were checked against yyjson 0.12.0; adding a flag out of order shifts
     every later one and silently enables the wrong behaviour.

     [YYJSON_READ_INSITU] (1 lsl 0) is deliberately absent: it makes yyjson
     write into the input buffer and requires YYJSON_PADDING_SIZE bytes of
     slack past [len], neither of which this binding can guarantee for an
     OCaml string. *)
  type t =
    | Stop_when_done
    | Allow_trailing_commas
    | Allow_comments
    | Allow_inf_and_nan
    | Number_as_raw
    | Allow_invalid_unicode
    | Bignum_as_raw
    | Allow_bom
    | Allow_ext_number
    | Allow_ext_escape
    | Allow_ext_whitespace
    | Allow_single_quoted_str
    | Allow_unquoted_key
  [@@deriving variants]

  let to_int = function
    | Stop_when_done -> 1 lsl 1
    | Allow_trailing_commas -> 1 lsl 2
    | Allow_comments -> 1 lsl 3
    | Allow_inf_and_nan -> 1 lsl 4
    | Number_as_raw -> 1 lsl 5
    | Allow_invalid_unicode -> 1 lsl 6
    | Bignum_as_raw -> 1 lsl 7
    | Allow_bom -> 1 lsl 8
    | Allow_ext_number -> 1 lsl 9
    | Allow_ext_escape -> 1 lsl 10
    | Allow_ext_whitespace -> 1 lsl 11
    | Allow_single_quoted_str -> 1 lsl 12
    | Allow_unquoted_key -> 1 lsl 13
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
  (* Bit positions must match YYJSON_WRITE_* in yyjson.h. *)
  type t =
    | Pretty
    | EscapeUnicode
    | EscapeSlashes
    | AllowInfAndNan
    | InfAndNanAsNull
    | AllowInvalidUnicode
    | PrettyTwoSpaces
    | NewlineAtEnd
    | LowercaseHex
  [@@deriving variants]

  let to_int = function
    | Pretty -> 1 lsl 0
    | EscapeUnicode -> 1 lsl 1
    | EscapeSlashes -> 1 lsl 2
    | AllowInfAndNan -> 1 lsl 3
    | InfAndNanAsNull -> 1 lsl 4
    | AllowInvalidUnicode -> 1 lsl 5
    | PrettyTwoSpaces -> 1 lsl 6
    | NewlineAtEnd -> 1 lsl 7
    | LowercaseHex -> 1 lsl 8 (* yyjson >= 0.13.0 *)
  ;;

  let to_int = List.fold_left ~init:0 ~f:(fun a x -> a lor to_int x)
end
