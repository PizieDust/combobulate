(* -*- combobulate-test-point-overlays: ((1 outline 360) (2 outline 374) (3 outline 399) (4 outline 439) (5 outline 482) (6 outline 547) (7 outline 600) (8 outline 711) (9 outline 799) (10 outline 878) (11 outline 987) (12 outline 1006) (13 outline 1078) (14 outline 1121) (15 outline 1148) (16 outline 1203)); eval: (combobulate-test-fixture-mode t); -*- *)

type user_id

type file_handle = int 

type 'a cache = ('a, float) Hashtbl.t 

type 'a internal_state = private 'a list 

type ('a, 'b) constrained_pair = 'a * 'b constraint 'a = string

type http_status = Success | NotFound | ServerError

type expression =
  | Const of int
  | Add of expression * expression
  | Multiply of expression * expression

type ui_event =
  | MouseClick of { x: int; y: int }
  | KeyPress of { key_code: int }

type _ value =
  | Int : int -> int value
  | String : string -> string value

type user_profile = {
  id: user_id;
  name: string;
  email: string option;
  mutable last_login: float;
}

type command = ..

type command +=
  | Login of { user: string; pass: string }
  | Logout

type command +=
  | SendMessage of string

exception Timeout_expired

exception Api_error of { code: int; message: string }

exception Old_error_name = Not_found
