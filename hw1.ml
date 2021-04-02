exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum n = if n = 1 then 1 else sum (n-1) + n;;
let rec fac n = if n = 1 then 1 else fac (n-1) * n;;
let rec fib n = if n = 0 || n = 1 then 1 else fib (n-1) + fib (n-2);;
let rec gcd m n = 
if m > n then gcd (m - n) n 
else if m < n then gcd m (n-m) 
else m;;
let rec max x =
match x with
[] -> 0
| h :: [] -> h
| h :: t -> let tmp = max t in
if tmp < h then h
else tmp;;
let rec sum_tree t =
match t with
Leaf n -> n
| Node (l, v, r) -> sum_tree l + v + sum_tree r;;
let rec depth t =
match t with
Node (l, v, r) -> 1 + depth l + depth r
| _ -> 0;;
let rec bin_search t x =
match t with
Leaf n -> if x = n then true else false
| Node (l, v, r) -> if x < v then bin_search l x
else if x > v then bin_search r x
else true;;
let rec preorder t =
match t with
Node (l, v, r) -> v :: (preorder l @ preorder r)
|Leaf n -> [n];;
let rec list_add m n =
match m, n with
h1 :: t1, h2 :: t2 -> (h1+h2)::(list_add t1 t2)
| [], _ -> n
| _, [] -> m;;
let rec insert m l =
match l with
[] -> [m]
| h :: t -> if h < m then h :: insert m t else m :: h :: t;;
let rec insort l =
match l with
h :: t -> insert h (insort t)
| [] -> [];;
let rec compose f g = fun x -> g ( f x);;
let rec curry f = fun x y -> f(x,y);; 
let rec uncurry f = fun (x, y) -> f x y;;
let rec multifun f n = fun x -> if n = 1 then f x else f ((multifun f (n-1)) x);;

let rec ltake l n =
match l, n with
_, 0 -> []
|h :: t, _ -> h :: ltake t (n-1)
|[], _ -> [];;
let rec lall f l =
match l with
[] -> true
| h :: t -> if f h then lall f t else false;;
let rec lmap f l =
match l with
[] -> []
| h :: t -> (f h) :: lmap f t;; 
let rec lrev l =
match l with
[] -> []
| h :: t -> (lrev t) @ [h];;
let rec lzip m n =
match m, n with
h1 :: t1, h2 :: t2 -> (h1, h2) :: (lzip t1 t2)
| [], _ -> []
| _, [] -> [];;
let rec split l =
match l with
[] -> ([], [])
| h1 :: h2 :: t -> let (l1, l2) = split t in ( h1 :: l1, h2 :: l2)
| [h] -> ([h], []);;

let rec cartprod m n =
match m with
[] -> []
| h :: t ->
(let rec prod x l =
match l with
[] -> []
| h :: t -> (x,h) :: prod x t in
prod h n) @ cartprod t n;;
