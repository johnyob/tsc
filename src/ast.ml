open! Core
module Ident = String
module Number = Float
module Key = String

module Literal = struct
  type t =
    | String of string
    | Number of Number.t
    | Boolean of bool
    | Null
  [@@deriving sexp, compare, equal]
end

module Function = struct
  module Parameter = struct
    type 'a t =
      { name : Ident.t
      ; type_ : 'a
      }
    [@@deriving sexp, equal, compare]
  end

  module Parameters = struct
    type 'a t =
      { parameters : 'a Parameter.t
      ; rest : 'a Parameter.t option
      }
    [@@deriving sexp, equal, compare]
  end

  type 'a t =
    { type_parameters : Ident.t list option
    ; parameters : 'a Parameters.t
    ; return : 'a
    }
  [@@deriving sexp, equal, compare]
end

module Object = struct
  module Property = struct
    type 'a t =
      { key : Key.t
      ; value : 'a
      }
    [@@deriving sexp, compare, equal]
  end

  type 'a t = { properties : 'a Property.t list } [@@deriving sexp, compare, equal]
end

module Type = struct
  module Application = struct
    type 'a t =
      { name : Ident.t
      ; arguments : 'a list
      }
    [@@deriving sexp, compare, equal]
  end

  type t =
    | Any
    | Void
    | Never
    | Unknown
    | Null
    | Number
    | String
    | Boolean
    | Function of t Function.t
    | Literal of Literal.t
    | Application of t Application.t
    | Object of t Object.t
    | Array of t
    | Union of t list (** Invariant: [List.length >= 2] *)
    | Intersection of t list
    | Tuple of t list
  [@@deriving sexp, equal, compare]
end

module Pattern = struct
  module Object = struct
    module Property = struct
      type 'a t =
        { key : Key.t
        ; pattern : 'a
        }
      [@@deriving equal, compare, sexp]
    end

    module Rest_property = struct
      type 'a t = { argument : 'a } [@@deriving equal, compare, sexp]
    end

    type 'a property =
      | Property of 'a Property.t
      | Rest_property of 'a Rest_property.t
    [@@deriving equal, compare, sexp]

    type 'a t =
      { properties : 'a property list
      ; annot : Type.t option
      }
    [@@deriving equal, compare, sexp]
  end

  module Array = struct
    module Element = struct
      type 'a t = { argument : 'a } [@@deriving equal, compare, sexp]
    end

    module Rest_element = struct
      type 'a t = { argument : 'a } [@@deriving equal, compare, sexp]
    end

    type 'a element =
      | Element of 'a Element.t
      | Rest_element of 'a Rest_element.t
    [@@deriving equal, compare, sexp]

    type 'a t =
      { elements : 'a element list
      ; annot : Type.t option
      }
    [@@deriving equal, compare, sexp]
  end

  module Ident = struct
    type t =
      { ident : Ident.t
      ; annot : Type.t option
      }
    [@@deriving equal, compare, sexp]
  end

  type t =
    | Object of t Object.t
    | Array of t Array.t
    | Ident of Ident.t
  [@@deriving equal, compare, sexp]
end

module Expression = struct
  module Or_spread = struct
    type 'a t =
      | Expression of 'a
      | Spread of 'a
    [@@deriving equal, compare, sexp]
  end

  module Array = struct
    type 'a t = { elements : 'a Or_spread.t list } [@@deriving equal, compare, sexp]
  end

  module Logical = struct
    type operator =
      | Or
      | And
    [@@deriving equal, compare, sexp]

    type 'a t =
      { operator : operator
      ; left : 'a
      ; right : 'a
      }
    [@@deriving equal, compare, sexp]
  end

  module Assignment = struct
    type operator =
      | Add_assign
      | Sub_assign
      | Mul_assign
      | Exp_assign
      | Div_assign
      | Mod_assign
    [@@deriving equal, compare, sexp]

    type 'a t =
      { operator : operator option
      ; left : Pattern.t
      ; right : 'a
      }
    [@@deriving equal, compare, sexp]
  end

  module Unary = struct
    type operator =
      | Sub
      | Add
      | Not
    [@@deriving equal, compare, sexp]

    type 'a t =
      { operator : operator
      ; argument : 'a
      }
    [@@deriving equal, compare, sexp]
  end

  module Binary = struct
    type operator =
      | Equal
      | Not_equal
      | Less_than
      | Less_than_equal
      | Greater_than
      | Greater_than_equal
      | Add
      | Sub
      | Mul
      | Exp
      | Div
      | Mod
      | In
    [@@deriving equal, compare, sexp]

    type 'a t =
      { operator : operator
      ; left : 'a
      ; right : 'a
      }
    [@@deriving equal, compare, sexp]
  end

  module Call = struct
    type 'a t =
      { callee : 'a
      ; type_arguments : Type.t list option
      ; arguments : 'a Or_spread.t
      }
    [@@deriving equal, compare, sexp]
  end

  module Conditional = struct
    type 'a t =
      { test : 'a
      ; consequent : 'a
      ; alternate : 'a
      }
    [@@deriving equal, compare, sexp]
  end

  module Member = struct
    module Property = struct
      type 'a t =
        | Key of Key.t
        | Expression of 'a
      [@@deriving equal, compare, sexp]
    end

    type 'a t =
      { _object : 'a
      ; property : 'a Property.t
      }
    [@@deriving equal, compare, sexp]
  end

  module Type_cast = struct
    type 'a t =
      { expression : 'a
      ; annot : Type.t
      }
    [@@deriving equal, compare, sexp]
  end

  type t =
    | Array of t Array.t
    | Arrow_function of t Function.t
    | Literal of Literal.t
    | Assignment of t Assignment.t
    | Unary of t Unary.t
    | Binary of t Binary.t
    | Logical of t Logical.t
    | Call of t Call.t
    | Conditional of t Conditional.t
    | Ident of Ident.t
    | Member of t Member.t
    | Object of t Object.t
    | Type_cast of t Type_cast.t
  [@@deriving equal, compare, sexp]
end
