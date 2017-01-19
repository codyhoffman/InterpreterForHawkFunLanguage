(* File Project/Inter.fs
    Interpreter for HawkFun, a small functional language.
    S. Knepper 12-16-2016
    C. Hoffman 12-16-2016

*)

module Inter

open Absyn 
open Env


let rec lookup env x =
    match env with 
    | []          -> failwith (x + " not found in environment")
    | (y, v) :: r -> if x = y then v else lookup r x


(* A runtime value is an integer, a list, or a function closure *)

type value = 
  | Int of int
  | List of value list
  | Closure of string option * string * expr * value env   

let rec toString v t = 
  match (v, t) with
  | (_, AnyT)          -> "value" 
  | (_, ArrowT (_, _)) -> "closure"  
  | (_, UnitT)         -> "null"
  | (Int i, IntT)      -> string i
  | (Int 0, BoolT)     -> "false"
  | (Int 1, BoolT)     -> "true"
  | (List l, ListT t1) -> "[" + listToString l t1 + "]"
  | _ ->  failwith "toString: mismatched type and value"
and listToString l t =
  match l with
  |      [] -> ""
  | v :: [] -> toString v t
  | v :: vs -> (toString v t) + "; " + (listToString vs t)


(* Interpreter begins here *)

let rec eval (e : expr) (env : value env) : value =
    match e with
    | (Con i, htype) -> Int i
    | (EListC, ListT t1) -> List []
    | (Var x, htype)-> lookup env x 
    | (Op1 (op, e1), h) -> 
      let v1 = eval e1 env
      match e1 with
      | (_,t) -> let ht = t in 
                 match (op, v1) with
                 | ("not", Int v1)         -> Int (if(v1 = 1) then 0 else 1)
                 | ("hd", List (hd::_))    -> (if(v1 <> List []) then hd else failwith "EXCEPTION: EMPTY LIST")
                 | ("tl", List (_::tl))    -> (if( v1 <> List []) then List tl else failwith "EXCEPTION: EMPTY LIST")
                 | ("ise", List [])        -> Int 1
                 | ("ise", List _)         -> Int 0
                 | ("print", v1)           -> Int(let x = (toString v1 ht) in System.Console.WriteLine(x) ; 0)
                 | _   -> failwith "unknown op"
    | (Op2 (op, e1, e2), htype)  -> 
      let i1 = eval e1 env in
      let i2 = eval e2 env in
      match (op, i1, i2) with
      | ("*", Int i1, Int i2)    -> Int (i1 * i2)
      | ("/", Int i1, Int i2)    -> Int (i1 / i2)
      | ("+", Int i1, Int i2)    -> Int (i1 + i2)
      | ("-", Int i1, Int i2)    -> Int (i1 - i2)
      | ("=", Int i1, Int i2)    -> Int (if i1 = i2 then 1 else 0)
      | ("<>", Int i1, Int i2)   -> Int (if i1 = i2 then 0 else 1)
      | ("<", Int i1, Int i2)    -> Int (if i1 < i2 then 1 else 0)
      | ("<=", Int i1, Int i2)   -> Int (if i1 <= i2 then 1 else 0)
      | ("::", i1, List i2)      -> List (i1 :: i2)
      | (";", Int i1, Int i2)    -> Int i2
      |  _     -> failwith "unknown binary op"
    
    | (If (e1, e2, e3), _) -> 
      match eval e1 env with
      | Int 0 -> eval e3 env
      | Int _ -> eval e2 env
      | _     -> failwith "eval If"

    | (Let (bind, e1), htype) ->
      match bind with 
      | (V (n, e2)) -> let nVal = eval e2 env  
                       let env2 = (n, nVal) :: env 
                       eval e1 env2    
      | (F(n, t, ht, e2)) -> 
        match t with 
        | (tn, tt) -> let env2 = (n, Closure(Some n, tn, e2, env)) :: env
                      eval e1 env2     
    | (Lam (tnam, e1), _) -> 
      match tnam with  
      | (n, t) -> Closure(None, n, e1, env)                                            
    | (Call((Var f, t1), eArg), _) -> 
      let fClosure = lookup env f in 
      match fClosure with
      | Closure (Some f, x, fBody, fDeclEnv) ->
        let xVal = eval eArg env in
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv in
        eval fBody fBodyEnv 
      | Closure (None, x, fBody, fDeclEnv) -> 
        let xVal = eval eArg env in
        let fBodyEnv = (x, xVal) :: (f, fClosure) :: fDeclEnv in
        eval fBody fBodyEnv
      | _ -> failwith "eval Call: not a function"
    | (Call(c, eArg), _) ->  
       match eval c env with 
       | Closure(_, y, e, env2) ->  let ex = eval eArg env in
                                    let nenv = (y, ex) :: env2
                                    eval e nenv
       | _ -> eval eArg env 

    | _ -> failwith "Program failed, invalid typing, syntax, or expression"

(* Evaluate in empty environment: program must have no free variables*)
let run e = eval e []

