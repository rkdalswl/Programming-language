(*
 * Call-by-value reduction   
 *)

exception NotImplemented 
exception Stuck

let freshVarCounter = ref 0
                          
(*   getFreshVariable : string -> string 
 *   use this function if you need to generate a fresh variable from s. 
 *)
let getFreshVariable s = 
  let _ = freshVarCounter := !freshVarCounter + 1
  in
  s ^ "__" ^ (string_of_int (!freshVarCounter))

let rec allFV e =
  match e with
    Uml.Var x -> [x]
    | Uml.Lam (x, e) -> List.fold_right (fun a b -> if a != x then a::b else b) (allFV(e)) []
    | Uml.App (e1, e2) -> List.fold_right (fun a b -> if List.mem a b then b else a::b) (allFV(e1)) (allFV(e2))

(* swap x y e = [x<->y]e *)
let rec swap x y e =
  match e with
    Uml.Var z -> if z = x then Uml.Var y
              else if z = y then Uml.Var x
              else Uml.Var z
    | Uml.Lam (z, e') -> if z = x then Uml.Lam (y, swap x y e')
                      else if z = y then Uml.Lam (x, swap x y e')
                      else Uml.Lam(z, swap x y e')
    | Uml.App (e1, e2) -> Uml.App (swap x y e1, swap x y e2)

(* substitute e' x e = [e'/x] e *)
let rec substitute e' x e =
  match e with
    Uml.Var z -> if x = z then e' else e
    | Uml.App (e1, e2) -> Uml.App(substitute e' x e1, substitute e' x e2)
    | Uml.Lam (y, e'') -> 
        if x = y 
          then e 
          else
            if List.mem y (allFV e') = false
              then Uml.Lam(y, substitute e' x e'')
              else let z = getFreshVariable "fresh" in Uml.Lam(z, substitute e' x (swap y z e''))

(*
 * implement a single step with reduction using the call-by-value strategy.
 *)
let rec stepv exp =         
  match exp with
    Uml.App (Uml.Lam(x, e), Uml.Lam(y, e')) -> substitute (Uml.Lam(y,e')) x e
    | Uml.App (Uml.Lam(x, e), e') -> Uml.App (Uml.Lam(x, e), (stepv e'))
    | Uml.App (e1, e2) -> Uml.App (stepv e1, e2)
    | _ -> raise Stuck  
  
(*
 * implement a single step with reduction using the call-by-name strategy.
 *)
let rec stepn exp = 
  match exp with
    Uml.App (Uml.Lam(x, e), e') -> substitute e' x e
    | Uml.App (e1, e2) -> Uml.App (stepn e1, e2)
    | _ -> raise Stuck       

let stepOpt stepf e = try Some (stepf e) with Stuck -> None

let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e

let stepStream stepf e =
  let rec steps e = 
    match (stepOpt stepf e) with 
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)


