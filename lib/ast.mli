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

module Print : sig
  val pp_program : program -> string
end
