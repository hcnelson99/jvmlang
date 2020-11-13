type unop = Negate

type binop =
  | Plus
  | Minus
  | Times
  | Divided_by
  | Modulo

type exp =
  | Const of Int32.t
  | Binop of
      { op : binop
      ; lhs : mexp
      ; rhs : mexp
      }
  | Unop of
      { op : unop
      ; operand : mexp
      }

and mexp = exp Mark.t

type program = exp

module Print = struct
  let pp_binop = function
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divided_by -> "/"
    | Modulo -> "%"
  ;;

  let pp_unop = function
    | Negate -> "-"
  ;;

  let rec pp_exp = function
    | Const n -> Int32.to_string n
    | Binop { op; lhs; rhs } -> [%string "(%{pp_mexp lhs} %{pp_binop op} %{pp_mexp rhs})"]
    | Unop { op; operand } -> [%string "(%{pp_unop op} %{pp_mexp operand})"]

  and pp_mexp mexp = pp_exp (Mark.data mexp)

  let pp_program = pp_exp
end
