open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

(*t is the token list*)
let rec parse_expr toks =
  match lookahead toks with
  | Some Tok_Let -> parse_LetExpr toks
  | Some Tok_If -> parse_IfExpr toks
  | Some Tok_Fun -> parse_FunExpr toks
  | _ -> parse_OrExpr toks
and parse_LetExpr tok = 
  match lookahead tok with
  | Some Tok_Let -> (let t = match_token tok Tok_Let in
    match lookahead t with
    | Some Tok_Rec -> (let t' = match_token t Tok_Rec in
      match lookahead t' with
      | Some Tok_ID(var) -> (let t'' = match_many t' [Tok_ID(var); Tok_Equal] in
        let (t''', expr1) = parse_expr t'' in
        let t'''' = match_token t''' Tok_In in
        let (t''''', expr2) = parse_expr t'''' in
        t''''', Let(var, true, expr1, expr2))
      | _ -> (raise (InvalidInputException "")))
    | Some Tok_ID(var) -> (let t' = match_many t [Tok_ID(var); Tok_Equal] in
      let (t'', expr1) = parse_expr t' in
      let t''' = match_token t'' Tok_In in
      let (t'''', expr2) = parse_expr t''' in
      t'''', Let(var, false, expr1, expr2))
    | _ -> (raise (InvalidInputException "")))
  | _ -> (raise (InvalidInputException ""))
and parse_FunExpr tok = 
  match lookahead tok with
  | Some Tok_Fun ->
    (match lookahead_many tok 1 with
    | Some Tok_ID(var) -> (let t = match_many tok [Tok_Fun; Tok_ID(var); Tok_Arrow] in
    let (t', expr) = parse_expr t in
    t', Fun(var, expr))
    | _ -> raise (InvalidInputException ""))
  | _ -> raise (InvalidInputException "")
and parse_IfExpr tok =
  match lookahead tok with
  | Some Tok_If ->
    let t = match_token tok Tok_If in
    let (t', expr1) = parse_expr t in
    let t'' = match_token t' Tok_Then in
    let (t''', expr2) = parse_expr t'' in
    let t'''' = match_token t''' Tok_Else in
    let (t''''', expr3) = parse_expr t'''' in
    t''''', If(expr1, expr2, expr3)
  | _ -> raise (InvalidInputException "")
and parse_OrExpr tok = 
  let (t, expr1) = parse_AndExpr tok in
  match lookahead t with
  | Some Tok_Or -> let t' = match_token t Tok_Or in
    let(t'', expr2) = parse_OrExpr t' in
  t'', Binop(Or, expr1, expr2)
  | _ -> t, expr1
and parse_AndExpr tok =
  let (t, expr1) = parse_EqExpr tok in
  match lookahead t with
  | Some Tok_And -> let t' = match_token t Tok_And in
    let(t'', expr2) = parse_AndExpr t' in
  t'', Binop(And, expr1, expr2)
  | _ -> t, expr1
and parse_EqExpr tok = 
  let (t, expr1) = parse_RelExpr tok in
  match lookahead t with
  | Some Tok_Equal -> let t' = match_token t Tok_Equal in
    let(t'', expr2) = parse_EqExpr t' in
    t'', Binop(Equal, expr1, expr2)
  | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
    let(t'', expr2) = parse_EqExpr t' in
    t'', Binop(NotEqual, expr1, expr2)
  | _ -> t, expr1
and parse_RelExpr tok =
  let (t, expr1) = parse_AddExpr tok in
  match lookahead t with
  | Some Tok_Less -> let t' = match_token t Tok_Less in
    let(t'', expr2) = parse_RelExpr t' in
    t'', Binop(Less, expr1, expr2)
  | Some Tok_Greater -> let t' = match_token t Tok_Greater in
    let(t'', expr2) = parse_RelExpr t' in
    t'', Binop(Greater, expr1, expr2)
  | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
    let(t'', expr2) = parse_RelExpr t' in
    t'', Binop(LessEqual, expr1, expr2)
  | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
    let(t'', expr2) = parse_RelExpr t' in
    t'', Binop(GreaterEqual, expr1, expr2)
  | _ -> t, expr1
and parse_AddExpr tok =
  let (t, expr1) = parse_MulExpr tok in
  match lookahead t with
  | Some Tok_Add -> let t' = match_token t Tok_Add in
    let(t'', expr2) = parse_AddExpr t' in
    t'', Binop(Add, expr1, expr2)
  | Some Tok_Sub -> let t' = match_token t Tok_Sub in
    let(t'', expr2) = parse_AddExpr t' in
    t'', Binop(Sub, expr1, expr2)
  | _ -> t, expr1
and parse_MulExpr tok =
  let (t, expr1) = parse_ConExpr tok in
  match lookahead t with
  | Some Tok_Mult -> let t' = match_token t Tok_Mult in
    let(t'', expr2) = parse_MulExpr t' in
    t'', Binop(Mult, expr1, expr2)
  | Some Tok_Div -> let t' = match_token t Tok_Div in
    let(t'', expr2) = parse_MulExpr t' in
    t'', Binop(Div, expr1, expr2)
  | _ -> t, expr1
and parse_ConExpr tok =
  let (t, expr1) = parse_UnExpr tok in
  match lookahead t with
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
    let(t'', expr2) = parse_ConExpr t' in
  t'', Binop(Concat, expr1, expr2)
  | _ -> t, expr1
and parse_UnExpr tok =
  match lookahead tok with
  | Some Tok_Not -> let t = match_token tok Tok_Not in
    let (t', expr1) = parse_UnExpr t in
    t', Not(expr1)
  | _ -> parse_AppExpr tok
and parse_AppExpr tok =
  let (t, expr1) = parse_SelExpr tok in
  match lookahead t with
  | Some Tok_Int(i) -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | Some Tok_Bool(b) -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | Some Tok_String(s) -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | Some Tok_ID(id) -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | Some Tok_LParen -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | Some Tok_LCurly -> let (t', expr2) = parse_PriExpr t in
  t', App(expr1, expr2)
  | _ -> t, expr1
and parse_SelExpr tok = 
  let (t, expr1) = parse_PriExpr tok in
  match lookahead t with
  | Some Tok_Dot -> (let t' = match_token t Tok_Dot in
    match lookahead t' with
    | Some Tok_ID(id) -> (let t'' = match_token t' (Tok_ID(id)) in
      t'', Select(Lab(id), expr1))
    | _ -> raise (InvalidInputException ""))
  | _ -> t, expr1
and parse_PriExpr tok =
  match lookahead tok with
  | Some Tok_Int(i) -> let t = match_token tok (Tok_Int(i)) in
    t, Int(i)
  | Some Tok_Bool(b) -> let t = match_token tok (Tok_Bool(b)) in
  t, Bool(b)
  | Some Tok_String(s) -> let t = match_token tok (Tok_String(s)) in
  t, String(s)
  | Some Tok_ID(id) -> let t = match_token tok (Tok_ID(id)) in
  t, ID(id)
  | Some Tok_LParen -> let t = match_token tok Tok_LParen in
  let (t', expr1) = parse_expr t in
  let t'' = match_token t' Tok_RParen in
  t'', expr1
  | Some Tok_LCurly -> parse_RecExpr tok
  | _ -> raise (InvalidInputException "")
and parse_RecExpr tok = 
  match lookahead tok with
  | Some Tok_LCurly -> 
    (let t = match_token tok Tok_LCurly in
    match lookahead t with
    | Some Tok_ID(id) -> (
      let (t'', expr1) = parse_RecBodExpr t in
      let t''' = match_token t'' Tok_RCurly in 
      t''', Record(expr1))
    | Some Tok_RCurly -> (let t' = match_token t Tok_RCurly in
      t', Record([]))
    | _ -> (raise (InvalidInputException "")))
  | _ -> raise (InvalidInputException "")
and parse_RecBodExpr tok =
  match lookahead tok with
  | Some Tok_ID(id) -> (let t = match_many tok [Tok_ID(id); Tok_Equal] in
    let (t', expr1) = parse_expr t in
    match lookahead t' with
    | Some Tok_Semi -> (let t'' = match_token t' Tok_Semi in
    let (t''', expr2) = parse_RecBodExpr t'' in
    t''', [(Lab(id), expr1)] @ expr2)
    | _ -> (t', [(Lab(id), expr1)]))
  | _ -> raise (InvalidInputException "")


(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_DefTop toks
  | Some Tok_DoubleSemi -> [], NoOp
  | _ -> parse_ExprTop toks
and parse_DefTop tok = 
  match lookahead tok with
  | Some Tok_Def -> (let t = match_token tok Tok_Def in
    match lookahead t with
    | Some Tok_ID(id) -> (let t' = match_many t [Tok_ID(id); Tok_Equal] in
      let (t'', expr1) = parse_expr t' in
      match lookahead t'' with
      | Some Tok_DoubleSemi -> (let t''' = match_token t'' Tok_DoubleSemi in
        t''', Def(id, expr1))
      | _ -> raise (InvalidInputException ""))
    | _ -> raise (InvalidInputException ""))
  | _ -> raise (InvalidInputException "")
and parse_ExprTop tok =
  let (t, expr1) = parse_expr tok in
  match lookahead t with 
  | Some Tok_DoubleSemi -> let t' = match_token t Tok_DoubleSemi in
    t', Expr(expr1)
  | _ -> raise (InvalidInputException "")
