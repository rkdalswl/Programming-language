exception NotImplemented
	    
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
						      
(** Recursive functions **)

let rec lconcat ll =
match ll with
h :: t -> h @ (lconcat t)
| [] -> [];; 

let rec lfoldl f e l =
match l with
h :: t -> lfoldl f (f (h, e)) t
| [] -> e;;					
			 
(** Tail recursive functions  **)

let fact n =
let rec fact_aux n acc =
if n=0 then acc
else fact_aux (n-1) acc*n
in fact_aux n 1;;

let power x n =
let rec power_aux x n acc =
if n=0 then acc
else power_aux x (n-1) acc*x
in power_aux x n 1;;

let fib n =
let rec fib_aux n a b =
if n=0 then 1
else if n =1 then b
else fib_aux (n-1) b (a+b) 
in fib_aux n 1 1;; 

let lfilter f l =
let rec lfilter_aux f l acc =
match l with
[] -> acc
| h::t -> if f h then lfilter_aux f t (acc@[h]) else lfilter_aux f t acc
in lfilter_aux f l [];;

let ltabulate n f =
let rec ltabulate_aux n f acc =
if n = -1 then acc
else ltabulate_aux (n-1) f [(f n)]@acc
in ltabulate_aux (n-1) f [];;

let rec union m n =
match m with
[] -> n
| h :: t ->
let rec exist x l = match l with [] -> false | h::t -> if h=x then true else exist x t in
if exist h n then union t n else union t ([h]@n);;

let inorder t =
  let rec inorder_aux (t : 'a tree) (post : 'a list) : 'a list =
    match t with
      Leaf n -> n::post
      | Node (l, v, r) ->
	let rec aux t = match t with Leaf n -> [n] | Node (l,v,r) -> (aux l) @ [v] @ (aux r) in
	let post' = [v] @ aux r @ post in
	inorder_aux l post'
  in inorder_aux t [];;
	   
let postorder t =
  let rec postorder_aux (t : 'a tree) (post : 'a list) : 'a list =
    match t with
      Leaf n -> n::post
      | Node (l, v, r) ->
        let rec aux t = match t with Leaf n -> [n] | Node (l,v,r) -> (aux l) @ (aux r) @ [v] in
        let post' = aux r @ [v] @ post in
        postorder_aux l post'
  in postorder_aux t [];;


let preorder t =
  let rec preorder_aux (t : 'a tree) (post : 'a list) : 'a list =
    match t with
      Leaf n -> post @ [n]
      | Node (l, v, r) ->
        let rec aux t = match t with Leaf n -> [n] | Node (l,v,r) -> [v] @ (aux l) @ (aux r) in
        let post' = post @ [v] @ aux l in
        preorder_aux r post'
  in preorder_aux t [];;
   
		       
(** Sorting in the ascending order **)

let rec quicksort l =
match l with
[] -> []
| h :: t ->
let rec split pv l = match l with [] -> ([], []) | h :: t -> let (l1, l2) = split pv t in if h < pv then (h::l1, l2) else (l1, h::l2) in
let (l1, l2) = split h t in
(quicksort l1) @ [h] @ (quicksort l2);;

let rec mergesort l =
match l with
[] -> []
| [h] -> [h]
| _ ->
let rec split l = match l with [] -> ([], []) | h1 :: h2 :: t -> let (l1, l2) = split t in ( h1 :: l1, h2 :: l2) | [h] -> ([h], []) in
let (l1, l2) = split l in
let l1' = mergesort l1 in
let l2' = mergesort l2 in
let rec merge l1 l2 =
match l1, l2 with
[], _ -> l2
|_, [] -> l1
|h1 :: t1, h2 :: t2 -> if h1 < h2 then h1 :: (merge t1 l2) else h2 :: (merge l1 t2) in
merge l1' l2';;

(** Structures **)

module type HEAP = 
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a 
    val update : 'a heap -> loc -> 'a -> 'a heap
  end
    
module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict 
  end

module Heap : HEAP =
  struct
    exception InvalidLocation 
		
    type loc = int       (* dummy type, to be chosen by students *) 
    type 'a heap = 'a list   (* dummy type, to be chosen by students *)

    let empty () = []
    let allocate h v = (h@[v], List.length h)
    let dereference h loc = if List.length h > loc then (List.nth h loc) 
				else raise InvalidLocation
    let update h loc v =
	let rec idx h loc v h' ck =
		match h with
		[] -> if ck = true then h' else raise InvalidLocation
		| cell :: t -> if List.length h'=loc then idx t loc v (h'@[v]) true
				 else idx t loc v (h'@[cell]) ck
	in idx h loc v [] false
  end
    
module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list
			      
    let empty () = []
    
    let rec lookup d k =
      match d with
        [] -> None
	| (k', v') :: t -> if k'=k then Some v' else lookup t k
	
    let delete d k =
      let rec delete_aux d k d' =
	match d with
          [] -> d'
	  | (k', v') :: t -> if k = k' then delete_aux t k d' 
				else delete_aux t k (d'@[(k',v')])
      in delete_aux d k []

    let insert d (k, v) =
	let rec insert_aux d (k,v) d' overlap =
		match d with
		[] -> if overlap = true then d' else d'@[(k,v)]
		| (k', v') :: t -> if k = k' then insert_aux t (k,v) (d'@[(k,v)]) true
					else insert_aux t (k,v) (d'@[(k',v')]) overlap
	in insert_aux d (k,v) [] false
  end
    
module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option
			     
    let empty () = fun key -> None
    let lookup d k = (d k)
    let delete d k = fun k' -> if k = k' then None else d k' 
    let insert d (k,v) = fun k' -> if k' = k then Some v else d k'
  end
