open Types

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)
let rec eval_expr env e = match e with
  | Int(i) -> Int(i)
  | Bool(b) -> Bool (b)
  | String(s) -> String (s)
  | ID(id) -> lookup env id
  | Not(expr) -> (match (eval_expr env expr) with
    | Bool(b) -> if (b = true) then Bool(false) else Bool(true)
    | _ -> raise (TypeError ("Expected type bool")))
  | Binop(Add, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Int(x + y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Sub, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Int(x - y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Mult, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Int(x * y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Div, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> if y != 0 then Int(x / y) else raise (DivByZeroError)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Greater, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Bool(x > y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Less, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Bool(x < y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(GreaterEqual, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Bool(x >= y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(LessEqual, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Int(x), Int(y) -> Bool(x <= y)
    | _ -> raise (TypeError ("Expected type int")))
  | Binop(Concat, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | String(x), String(y) -> String(x^y)
    | _ -> raise (TypeError ("Expected type string")))
  | Binop(Equal, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | String(x), String(y) -> Bool(x = y)
    | Int(x), Int(y) -> Bool(x = y)
    | Bool(x), Bool(y) -> Bool(x = y)
    | _ -> raise (TypeError ("Cannot compare types")))
  | Binop(NotEqual, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | String(x), String(y) -> Bool(x != y)
    | Int(x), Int(y) -> Bool(x != y)
    | Bool(x), Bool(y) -> Bool(x != y)
    | _ -> raise (TypeError ("Cannot compare types")))
  | Binop(Or, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Bool(x), Bool(y) -> Bool(x || y)
    | _ -> raise (TypeError ("Expected type bool")))
  | Binop(And, expr1, expr2) -> (match (eval_expr env expr1), (eval_expr env expr2) with
    | Bool(x), Bool(y) -> Bool(x && y)
    | _ -> raise (TypeError ("Expected type bool")))
  | If(expr1, expr2, expr3) -> (match (eval_expr env expr1) with
    | Bool(x) -> if x = true then (eval_expr env expr2) else (eval_expr env expr3)
    | _ -> raise (TypeError ("Expected type bool")))
  | Let(v, bool, expr1, expr2) ->
    (if bool = false then 
      let ex = eval_expr env expr1 in
      let new_env = extend env v ex in
      eval_expr new_env expr2
    else 
      let tmp_env = extend_tmp env v in
      let expr1 = eval_expr tmp_env expr1 in
      (update tmp_env v expr1);
      eval_expr tmp_env expr2)
  | Fun(v, expr) -> Closure(env, v, expr)
  | App(expr1, expr2) -> (match (eval_expr env expr1) with
    | Closure(a, x, e) ->
      let v = eval_expr env expr2 in
      let new_env = extend a x v in
      eval_expr new_env e
    | _ -> raise (TypeError ("Not a function")))
  | Record(list) -> 
    Record(list)
  | Select(label, expr) -> (match (eval_expr env expr) with
    | Record(lst) ->
      let rec f l = match l with
      | [] -> raise (SelectError ("Label not found"))
      | (lab, ex)::t -> if lab = label then (eval_expr env ex) else f t in
      f lst 
    | _ -> raise (TypeError ("Not a record")))
  | Closure(env, v, expr) ->  Closure(env, v, expr)

  

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = match m with
  | Def(v, expr) -> let tmp_env = extend_tmp env v in
      let ex = eval_expr tmp_env expr in
      (update tmp_env v ex);
      (tmp_env, Some (ex))
  | Expr(expr) -> env, Some (eval_expr env expr)
  | NoOp -> env, (None)
