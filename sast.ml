include Ast

type expr_t = 
    Int_Literal_t of int
  | Double_Literal_t of double
  | Bool_Literal_t of bool
  | Char_Literal_t of char

type stmt_t = 
    Expr_t of expr_t
  | Return_t of expr_t 