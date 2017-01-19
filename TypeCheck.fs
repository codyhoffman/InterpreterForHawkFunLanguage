 (* File Project/TypeCheck.fs 
    Type checker for HawkFun, a small functional language.
    S. Knepper 2016-12-16
    C. Hoffman 2016-12-16
  *)

module TypeCheck

open Env 
open Absyn


(* Type checking for the functional language: *)
let rec checkType (e : expr) (env : htype env) : expr =
    match e with
    | (Con i, IntT) -> (Con i, IntT) 
    | (Con i, BoolT) -> (Con i, BoolT)
    | (Con i, UnitT) -> (Con i, UnitT)
    | (Var x, _)  -> (Var x, lookup env x)
    | (EListC, ListT (t1)) -> (EListC, ListT t1)
    | (Op1(ope, e), _) -> 
      let t = checkType e env 
      match (ope, t) with
      | ("not", (v, BoolT))    -> (Op1("not", (v, BoolT)), BoolT)
      | ("hd", (e1, ListT t))  -> (Op1("hd", (e1, ListT t)), t) 
      | ("tl", (e1, ListT t))  -> (Op1("tl", (e1, ListT t)), ListT t)
      | ("ise", (e1, ListT t)) -> (Op1("ise", (e1, ListT t)), BoolT)
      | ("print", e1) -> (Op1("print", e1), UnitT)
      | _   -> failwith "unknown op, or type error"
    | (Op2(ope, e1, e2), _) -> 
      let t1 = checkType e1 env
      let t2 = checkType e2 env
      match (ope, t1, t2) with
      | ("*", (x, IntT), (y, IntT)) -> (Op2("*", (x, IntT), (y, IntT)), IntT)
      | ("/", (x, IntT), (y, IntT)) -> (Op2("/", (x, IntT), (y, IntT)), IntT)
      | ("+", (x, IntT), (y, IntT)) -> (Op2("+", (x, IntT), (y, IntT)), IntT)
      | ("-", (x, IntT), (y, IntT)) -> (Op2("-", (x, IntT), (y, IntT)), IntT)
      | ("=", (x, IntT), (y, IntT)) -> (Op2("=", (x, IntT), (y, IntT)), BoolT)
      | ("<", (x, IntT), (y, IntT)) -> (Op2("<", (x, IntT), (y, IntT)), BoolT)
      | ("<>", (x, IntT), (y, IntT)) -> (Op2("<>", (x, IntT), (y, IntT)), BoolT)
      | ("<=", (x, IntT), (y, IntT)) -> (Op2("<=", (x, IntT), (y, IntT)), BoolT)
      | ("::", (x, lt1), (y, lt2)) -> 
        if lt1 = lt2 || lt2 = ListT lt1  
        then (Op2("::", (x, lt1), (y, lt2)), ListT lt1) 
        else failwith "list type error"
      | (";", x, (y, t)) -> (Op2(";", x, (y, t)), t) 
      | _   -> failwith "unknown binary op, or type error"
    | (If(e1, e2, e3), _) ->
      let ne1 = checkType e1 env 
      match ne1 with
      | (_, BoolT) -> let t2 = checkType e2 env
                      let t3 = checkType e3 env
                      match t2, t3 with 
                      | (_, ift1), (_, ift2) -> if ift1 = ift2
                                                then (If(ne1, t2, t3), ift2)
                                                else failwith "If: branch types differ"
      | _    -> failwith "If: condition not boolean"
    | (Let(b, e), _) -> 
      match b with 
      | (V (n, e1)) -> let e2 = checkType e1 env 
                       match e2 with 
                       | (_, t1) -> let be = (n, t1) :: env
                                    let ne = checkType e be
                                    match ne with 
                                    | (_, nt) -> (Let(V (n, e2), ne),nt) //UNSURE ABOUT TYPE RULES         
      | (F(n, tn, t1, e1)) -> 
        match tn with 
        | (arg, argt) -> let be = (arg, argt) :: (n, ArrowT(argt, t1)) :: env 
                         let e2 = checkType e1 be   
                         let e3 = checkType e be 
                         (Let(F (n, tn, t1, e2), e3), t1) 
    | (Lam(tName, e), _) -> 
      match tName with 
      | (n, t) -> let be = (n, t) :: env 
                  let fe = checkType e be
                  match fe with 
                  | (_, ft) -> (Lam(tName, fe), ArrowT(t, ft))

    | (Call((Var f, t1), eArg), _) -> 
      let e1 = lookup env f 
      match e1 with
      | ArrowT(xTyp, rTyp) -> let ne = checkType eArg env 
                              match ne with 
                              | (_, t) -> if t = xTyp then (Call((Var f, e1), ne), rTyp) 
                                          else failwith "Call: wrong argument type"
      | _ -> failwith "Call: unknown function"  
    | (Call(c , eArg), _) -> let ne = checkType eArg env
                             match ne with  
                             | (_, t) -> (Call(checkType c env, ne), t) 
    
    | _ -> failwith "Invalid Type"

let check e = checkType e []