type read_flag =
  | No_read_flag
  | Insitu
  | Stop_when_done
  | Allow_trailing_commas
  | Allow_inf_and_nan
  | Number_as_raw
  | Allow_invalid_unicode

type read_code =
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

type doc
type va
type mutdoc
type mutval
type value

include Json_repr.Repr with type value := value

type version =
  { major : int
  ; minor : int
  ; patch : int
  }

val version : version lazy_t
val free_doc : doc -> unit
val free_mut_doc : mutdoc -> unit
val value_of_doc : Bigstringaf.t -> doc -> value

(** Use it at most one time only in toplevel values *)
val free_value : value -> unit

val of_file : ?flags:read_flag list -> buf:Bigstringaf.t -> string -> doc

val of_bigstring
  :  ?flags:read_flag list
  -> ?pos:int
  -> ?len:int
  -> buf:Bigstringaf.t
  -> Bigstringaf.t
  -> doc

val of_string
  :  ?flags:read_flag list
  -> ?pos:int
  -> ?len:int
  -> buf:Bigstringaf.t
  -> string
  -> doc

type write_flag =
  | NoWriteFlag
  | Pretty
  | EscapeUnicode
  | EscapeSlashes
  | AllowInfAndNan
  | InfAndNanAsNull
  | AllowInvalidUnicode

val to_file : ?flags:write_flag list -> buf:Bigstringaf.t -> string -> mutdoc -> unit
val file_of_value : ?flags:write_flag list -> buf:Bigstringaf.t -> string -> value -> unit
val value_of_string : buf:Bigstringaf.t -> string -> value

val bigstring_of_value
  :  ?flags:write_flag list
  -> buf:Bigstringaf.t
  -> value
  -> Bigstringaf.t
