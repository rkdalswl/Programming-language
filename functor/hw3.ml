open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y 
end

(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool 
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = x || y
  let ( ** ) x y = x && y
  let (==) x y = x = y
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = Empty | V of elem * t

  exception VectorIllegal

  let create l = List.fold_right (fun a b -> V(a,b)) l Empty
  let rec to_list v =
    match v with
      Empty -> [] | V(a,b) -> a :: to_list b
  let rec dim v =
    match v with
      Empty -> 0 | V(a, b) -> 1 + dim b
  let rec nth v n =
    match v with
      Empty -> raise VectorIllegal | V(a,b) -> if n = 0 then a else nth b (n-1)
  let rec (++) x y =
    match x, y with 
      | V(a, b), V(c, d) -> V((Scal.(++) a c), b ++ d)
      | Empty, Empty -> Empty
      | ((Empty, V (_, _))|(V (_, _), Empty)) -> raise VectorIllegal
  let rec (==) x y =
    match x, y with
      V(a,b), V(c,d) -> if Scal.(==) a c then b == d else false
      | Empty, Empty -> true
      | ((Empty, V (_, _))|(V (_, _), Empty)) -> raise VectorIllegal  
  let rec innerp x y =
    match x, y with
      V(a,b), V(c,d) -> Scal.(++) (Scal.( ** ) a c) (innerp b d)
      | Empty, Empty -> Scal.zero
      | ((Empty, V (_, _))|(V (_, _), Empty)) -> raise VectorIllegal
end

(* Problem 1-3 *)
(* Matrices *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  type elem = Scal.t
  module VectorModule = VectorFn(Scal)
  type t = Empty | M of VectorModule.t * t

  exception MatrixIllegal

  let rec create ll = 
    if (List.length ll != 0) && (List.fold_left (fun a b -> a && (List.length ll = List.length b)) true ll) then 
      List.fold_right (fun a b -> M(VectorModule.create a, b)) ll Empty (**)
    else raise MatrixIllegal

  let identity k =
    if k <= 0 then raise MatrixIllegal
    else
    let rec aux2 m n accum = match n with 0 -> accum | a -> if a = m then aux2 m (n-1) (Scal.one::accum) else aux2 m (n-1) (Scal.zero::accum) in
    let rec aux k m accum = match m with 0 -> accum | _ -> aux k (m-1) ((aux2 m k [])::accum) in
    let aux_ll = aux k k [] in 
    create aux_ll

  let rec dim m = match m with Empty -> 0 | M(a,b) -> 1 + dim b

  let transpose m = 
   let rec aux m accum =
      match m with
        M(a, b) -> let v = VectorModule.to_list a in
		   let l = List.fold_right (fun a b -> [a]::b) v [] in
		   let rec aux2 l1 l2 = match l1,l2 with h1::t1, h2::t2 -> (h1@h2)::(aux2 t1 t2)
                                        | [], _::_ -> l2 | _, [] -> [] in
                   aux b (aux2 accum l)
	| Empty -> accum
    in create (aux m []) 	

  let rec to_list m =
    match m with
      M(a, b) -> let v = VectorModule.to_list a in v::(to_list b)
      | Empty -> []

  let rec get m r c =
    if r < 0 || c < 0 then raise MatrixIllegal
    else match r,m with
      0,M(a,b) -> VectorModule.nth a c | _, M(a,b) -> get b (r-1) c | _, Empty -> raise MatrixIllegal

  let rec (++) x y =
    match x, y with
      M(a,b), M(c,d) -> M((VectorModule.(++) a c), b ++ d)
      | Empty, Empty -> Empty
      | ((Empty, M (_, _))|(M (_, _), Empty)) -> raise MatrixIllegal

  let ( ** ) x y =
    if (dim x) != (dim y) then  raise MatrixIllegal
    else
     let y' = transpose y in
     let rec aux2 a y = match y with M(c,d) -> (VectorModule.( innerp ) a c)::(aux2 a d)|_->[] in 
     let rec aux x y = match x,y with M(a,b), M(c,d) -> (aux2 a y)::(aux b y) | _, _ -> [] in
     create(aux x y')
  
  let rec (==) x y =
    match x,y with
      M(a,b), M(c,d) -> (VectorModule.(==) a c) && b == d
      | Empty, Empty -> true
      | ((Empty, M (_, _))|(M (_, _), Empty)) -> raise MatrixIllegal  
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure a =
    let d = Mat.dim a in
    let id = Mat.identity d in
    let rec aux clo a =
      let next = Mat.(++) (Mat.( ** ) clo a) id in
      if Mat.(==) clo next then clo else aux next a
    in aux a a
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach m = BoolMat.to_list(BoolMatClosure.closure (BoolMat.create m))

let al = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = (-1)              (* Dummy value : Rewrite it! *)
  let one = 0               (* Dummy value : Rewrite it! *)

  let (++) x y = if x != (-1) && y != (-1) then min x y else max x y
  let ( ** ) x y = if x != (-1) && y != (-1) then x+y else -1
  let (==) x y = x = y
end

(* .. Write some code here .. *)
module DistMat = MatrixFn(Distance)
module DistMatClosure = ClosureFn(DistMat)

let distance m = DistMat.to_list(DistMatClosure.closure (DistMat.create m))

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0              (* Dummy value : Rewrite it! *)
  let one = (-1)               (* Dummy value : Rewrite it! *)
 
  let (++) x y = if x != (-1) && y != (-1) then max x y else min x y
  let ( ** ) x y = if x != (-1) && y != (-1) then min x y else max x y
  let (==) x y = x = y
end

(* .. Write some code here .. *)
module WeightMat = MatrixFn(Weight)
module WeightMatClosure = ClosureFn(WeightMat)

let weight m = WeightMat.to_list(WeightMatClosure.closure (WeightMat.create m))

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try 
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" 

