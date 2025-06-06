(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

type 'a cmp = 'a -> 'a -> int
type 'a eq = 'a -> 'a -> bool

include List

(** Tail-rec implementation of usual functions. This is a well-known trick used
    in, for instance, ExtLib and Batteries. *)

type 'a cell = {
  head : 'a;
  mutable tail : 'a list;
}

external cast : 'a cell -> 'a list = "%identity"

(** Extensions and redefinitions of OCaml Stdlib *)

(** {6 Equality, testing} *)

let rec same_length l1 l2 = match l1, l2 with
| [], [] -> true
| _ :: l1, _ :: l2 -> same_length l1 l2
| ([], _ :: _) | (_ :: _, []) -> false

let rec compare cmp l1 l2 =
  if l1 == l2 then 0 else
  match l1,l2 with
  | [], [] -> 0
  | _::_, [] -> 1
  | [], _::_ -> -1
  | x1::l1, x2::l2 ->
    match cmp x1 x2 with
    | 0 -> compare cmp l1 l2
    | c -> c

let rec equal cmp l1 l2 =
  l1 == l2 ||
  match l1, l2 with
  | [], [] -> true
  | x1 :: l1, x2 :: l2 -> cmp x1 x2 && equal cmp l1 l2
  | _ -> false

let is_empty = function
  | [] -> true
  | _ -> false

let mem_f cmp x l =
  List.exists (cmp x) l

let for_all_i p =
  let rec for_all_p i = function
    | [] -> true
    | a::l -> p i a && for_all_p (i+1) l
  in
  for_all_p

let for_all2eq f l1 l2 =
  try List.for_all2 f l1 l2 with Invalid_argument _ -> false

let exists_i p =
  let rec exists_p i = function
    | [] -> false
    | a::l -> p i a || exists_p (i+1) l
  in
  exists_p

let prefix_of cmp prefl l =
  let rec prefrec = function
    | (h1::t1, h2::t2) -> cmp h1 h2 && prefrec (t1,t2)
    | ([], _) -> true
    | _ -> false
  in
  prefrec (prefl,l)

(** {6 Creating lists} *)

let interval n m =
  let rec interval_n (l,m) =
    if n > m then l else interval_n (m::l, pred m)
  in
  interval_n ([], m)

let addn n v =
  let rec aux n l =
    if Int.equal n 0 then l
    else aux (pred n) (v :: l)
  in
  if n < 0 then invalid_arg "List.addn"
  else aux n

let make n v =
  addn n v []

let rec init_loop len f p i =
  if Int.equal i len then ()
  else
    let c = { head = f i; tail = [] } in
    p.tail <- cast c;
    init_loop len f c (succ i)

let init len f =
  if len < 0 then invalid_arg "List.init"
  else if Int.equal len 0 then []
  else
    let c = { head = f 0; tail = [] } in
    init_loop len f c 1;
    cast c

let rec append_loop p tl = function
  | [] -> p.tail <- tl
  | x :: l ->
    let c = { head = x; tail = [] } in
    p.tail <- cast c;
    append_loop c tl l

let append l1 l2 = match l1 with
  | [] -> l2
  | x :: l ->
    let c = { head = x; tail = [] } in
    append_loop c l2 l;
    cast c

let rec copy p = function
  | [] -> p
  | x :: l ->
    let c = { head = x; tail = [] } in
    p.tail <- cast c;
    copy c l

let rec concat_loop p = function
  | [] -> ()
  | x :: l -> concat_loop (copy p x) l

let concat l =
  let dummy = { head = Obj.magic 0; tail = [] } in
  concat_loop dummy l;
  dummy.tail

let flatten = concat

(** {6 Lists as arrays} *)

let assign l n e =
  let rec assrec stk l i = match l, i with
    | (h :: t, 0) -> List.rev_append stk (e :: t)
    | (h :: t, n) -> assrec (h :: stk) t (pred n)
    | ([], _) -> failwith "List.assign"
  in
  assrec [] l n

(** {6 Filtering} *)

(* [filter_loop f (Some (c0,l0)) c l] will do c0.tail <- l0 if [for_all f l] *)
let rec filter_loop f reset p = function
  | [] -> begin match reset with
      | None -> ()
      | Some (c,orig) -> c.tail <- orig
    end
  | x :: l as orig ->
    if f x then
      let c = { head = x; tail = [] } in
      let () = p.tail <- cast c in
      let reset = match reset with
        | Some _ -> reset
        | None -> Some (p,orig)
      in
      filter_loop f reset c l
    else
      filter_loop f None p l

let rec filter f = function
  | [] -> []
  | x :: l' as orig ->
    if f x then
      let c = { head = x; tail = [] } in
      filter_loop f None c l';
      if c.tail == l' then orig else cast c
    else
      filter f l'

let rec filter2_loop f p q l1 l2 = match l1, l2 with
  | [], [] -> ()
  | x :: l1', y :: l2' ->
    let b = f x y in
    filter2_loop f p q l1' l2';
    if b then
      if p.tail == l1' then begin
          p.tail <- l1;
          q.tail <- l2
        end
      else begin
          p.tail <- x :: p.tail;
          q.tail <- y :: q.tail
        end
  | _ -> invalid_arg "List.filter2"

let rec filter2 f l1 l2 = match l1, l2 with
  | [], [] -> ([],[])
  | x1 :: l1', x2 :: l2' ->
    let b = f x1 x2 in
    if b then
      let c1 = { head = x1; tail = [] } in
      let c2 = { head = x2; tail = [] } in
      filter2_loop f c1 c2 l1' l2';
      if c1.tail == l1' then (l1, l2) else (cast c1, cast c2)
    else
      filter2 f l1' l2'
  | _ -> invalid_arg "List.filter2"

let filteri p =
  let rec filter_i_rec i = function
    | [] -> []
    | x :: l -> let l' = filter_i_rec (succ i) l in if p i x then x :: l' else l'
  in
  filter_i_rec 0

let rec filter_with_loop filter p l = match filter, l with
  | [], [] -> ()
  | b :: filter, x :: l' ->
    filter_with_loop filter p l';
    if b then if p.tail == l' then p.tail <- l else p.tail <- x :: p.tail
  | _ -> invalid_arg "List.filter_with"

let rec filter_with filter l = match filter, l with
  | [], [] -> []
  | b :: filter, x :: l' ->
    if b then
      let c = { head = x; tail = [] } in
      filter_with_loop filter c l';
      if c.tail == l' then l else cast c
    else filter_with filter l'
  | _ -> invalid_arg "List.filter_with"

let rec map_filter_loop f p = function
  | [] -> ()
  | x :: l ->
    match f x with
    | None -> map_filter_loop f p l
    | Some y ->
      let c = { head = y; tail = [] } in
      p.tail <- cast c;
      map_filter_loop f c l

let rec map_filter f = function
  | [] -> []
  | x :: l' ->
    match f x with
    | None -> map_filter f l'
    | Some y ->
      let c = { head = y; tail = [] } in
      map_filter_loop f c l';
      cast c

let rec map_filter_i_loop f i p = function
  | [] -> ()
  | x :: l ->
    match f i x with
    | None -> map_filter_i_loop f (succ i) p l
    | Some y ->
      let c = { head = y; tail = [] } in
      p.tail <- cast c;
      map_filter_i_loop f (succ i) c l

let rec map_filter_i_loop' f i = function
  | [] -> []
  | x :: l' ->
    match f i x with
    | None -> map_filter_i_loop' f (succ i) l'
    | Some y ->
      let c = { head = y; tail = [] } in
      map_filter_i_loop f (succ i) c l';
      cast c

let map_filter_i f l =
  map_filter_i_loop' f 0 l

let partitioni p =
  let rec aux i = function
    | [] -> [], []
    | x :: l ->
      let (l1, l2) = aux (succ i) l in
      if p i x then (x :: l1, l2)
      else (l1, x :: l2)
  in
  aux 0

(** {6 Applying functorially} *)

let rec map_loop f p = function
  | [] -> ()
  | x :: l ->
    let c = { head = f x; tail = [] } in
    p.tail <- cast c;
    map_loop f c l

let map f = function
  | [] -> []
  | x :: l ->
    let c = { head = f x; tail = [] } in
    map_loop f c l;
    cast c

let rec map2_loop f p l1 l2 = match l1, l2 with
  | [], [] -> ()
  | x :: l1, y :: l2 ->
    let c = { head = f x y; tail = [] } in
    p.tail <- cast c;
    map2_loop f c l1 l2
  | _ -> invalid_arg "List.map2"

let map2 f l1 l2 = match l1, l2 with
  | [], [] -> []
  | x :: l1, y :: l2 ->
    let c = { head = f x y; tail = [] } in
    map2_loop f c l1 l2;
    cast c
  | _ -> invalid_arg "List.map2"

(* remove when requiring OCaml >= 5.1.0 *)
let rec concat_map_loop f p = function
  | [] -> ()
  | x :: l -> concat_map_loop f (copy p (f x)) l

(* remove when requiring OCaml >= 5.1.0 *)
let concat_map f l =
  let dummy = { head = Obj.magic 0; tail = [] } in
  concat_map_loop f dummy l;
  dummy.tail

(** Like OCaml [List.mapi] but tail-recursive *)

let rec map_i_loop f i p = function
  | [] -> ()
  | x :: l ->
    let c = { head = f i x; tail = [] } in
    p.tail <- cast c;
    map_i_loop f (succ i) c l

let map_i f i = function
  | [] -> []
  | x :: l ->
    let c = { head = f i x; tail = [] } in
    map_i_loop f (succ i) c l;
    cast c

let map_left = map

let map2_i f i l1 l2 =
  let rec map_i i = function
    | ([], []) -> []
    | (h1 :: t1, h2 :: t2) -> let v = f i h1 h2 in v :: map_i (succ i) (t1,t2)
    | (_, _) -> invalid_arg "map2_i"
  in
  map_i i (l1,l2)

let rec map3_loop f p l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> ()
  | x :: l1, y :: l2, z :: l3 ->
    let c = { head = f x y z; tail = [] } in
    p.tail <- cast c;
    map3_loop f c l1 l2 l3
  | _ -> invalid_arg "List.map3"

let map3 f l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> []
  | x :: l1, y :: l2, z :: l3 ->
    let c = { head = f x y z; tail = [] } in
    map3_loop f c l1 l2 l3;
    cast c
  | _ -> invalid_arg "List.map3"

let rec map4_loop f p l1 l2 l3 l4 = match l1, l2, l3, l4 with
  | [], [], [], [] -> ()
  | x :: l1, y :: l2, z :: l3, t :: l4 ->
    let c = { head = f x y z t; tail = [] } in
    p.tail <- cast c;
    map4_loop f c l1 l2 l3 l4
  | _ -> invalid_arg "List.map4"

let map4 f l1 l2 l3 l4 = match l1, l2, l3, l4 with
  | [], [], [], [] -> []
  | x :: l1, y :: l2, z :: l3, t :: l4 ->
    let c = { head = f x y z t; tail = [] } in
    map4_loop f c l1 l2 l3 l4;
    cast c
  | _ -> invalid_arg "List.map4"

let rec map_until_loop f p = function
  | [] -> []
  | x :: l as l' ->
    match f x with
    | None -> l'
    | Some fx ->
      let c = { head = fx; tail = [] } in
      p.tail <- cast c;
      map_until_loop f c l

let map_until f = function
  | [] -> [], []
  | x :: l as l' ->
    match f x with
    | None -> [], l'
    | Some fx ->
    let c = { head = fx; tail = [] } in
    let l = map_until_loop f c l in
    cast c, l

let rec map_of_array_loop f p a i l =
  if Int.equal i l then ()
  else
    let c = { head = f (Array.unsafe_get a i); tail = [] } in
    p.tail <- cast c;
    map_of_array_loop f c a (i + 1) l

let map_of_array f a =
  let l = Array.length a in
  if Int.equal l 0 then []
  else
    let c = { head = f (Array.unsafe_get a 0); tail = [] } in
    map_of_array_loop f c a 1 l;
    cast c

let map_append f l = flatten (map f l)

let map_append2 f l1 l2 = flatten (map2 f l1 l2)

let rec extend l a l' = match l,l' with
  | true :: l, b :: l' -> b :: extend l a l'
  | false :: l, l' -> a :: extend l a l'
  | [], [] -> []
  | _ -> invalid_arg "extend"

let count f l =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> if f h then aux (acc + 1) t else aux acc t
  in
  aux 0 l

(** {6 Finding position} *)

let rec index_f f x l n = match l with
  | [] -> raise Not_found
  | y :: l -> if f x y then n else index_f f x l (succ n)

let index f x l = index_f f x l 1

let index_opt f x l = try Some (index f x l) with Not_found -> None

let index0 f x l = index_f f x l 0

(** {6 Folding} *)

let fold_left_until f accu s =
  let rec aux accu = function
    | [] -> accu
    | x :: xs -> match f accu x with CSig.Stop x -> x | CSig.Cont i -> aux i xs
  in
  aux accu s

let fold_right_i f i l =
  let rec it_f i l a = match l with
    | [] -> a
    | b :: l -> f (i-1) b (it_f (i-1) l a)
  in
  it_f (List.length l + i) l

let fold_left_i f =
  let rec it_list_f i a = function
    | [] -> a
    | b :: l -> it_list_f (i+1) (f i a b) l
  in
  it_list_f

let rec fold_left3 f accu l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> accu
  | (a1 :: l1, a2 :: l2, a3 :: l3) -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
  | (_, _, _) -> invalid_arg "List.fold_left3"

let rec fold_left4 f accu l1 l2 l3 l4 =
  match (l1, l2, l3, l4) with
  | ([], [], [], []) -> accu
  | (a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4) -> fold_left4 f (f accu a1 a2 a3 a4) l1 l2 l3 l4
  | (_, _, _, _) -> invalid_arg "List.fold_left4"

let rec fold_left5 f accu l1 l2 l3 l4 l5 =
  match (l1, l2, l3, l4, l5) with
  | ([], [], [], [], []) -> accu
  | (a1 :: l1, a2 :: l2, a3 :: l3, a4 :: l4, a5 :: l5) -> fold_left5 f (f accu a1 a2 a3 a4 a5) l1 l2 l3 l4 l5
  | (_, _, _, _, _) -> invalid_arg "List.fold_left5"

(* [fold_right_and_left f [a1;...;an] hd =
   f (f (... (f (f hd
                   an
                   [an-1;...;a1])
              an-1
              [an-2;...;a1])
         ...)
        a2
        [a1])
     a1
     []] *)

let fold_right_and_left f l hd =
  let rec aux tl = function
    | [] -> hd
    | a :: l -> let hd = aux (a :: tl) l in f hd a tl
  in
  aux [] l

(* Match sets as lists according to a matching function, also folding a side effect *)
let rec fold_left2_set e f x l1 l2 =
  match l1 with
  | a1 :: l1 ->
      let rec find seen = function
        | [] -> raise e
        | a2 :: l2 ->
            try fold_left2_set e f (f x a1 a2 l1 l2) l1 (List.rev_append seen l2)
            with e' when e' = e -> find (a2 :: seen) l2 in
      find [] l2
  | [] ->
      if l2 = [] then x else raise e

(* Poor man's monadic map *)
let rec fold_left_map f e = function
  | []  -> (e,[])
  | h :: t ->
    let e',h' = f e h in
    let e'',t' = fold_left_map f e' t in
    e'',h' :: t'

(* (* tail-recursive version of the above function *)
let fold_left_map f e l =
  let g (e,b') h =
    let (e',h') = f e h in
      (e',h'::b')
  in
  let (e',lrev) = List.fold_left g (e,[]) l in
    (e',List.rev lrev)
*)

(* The same, based on fold_right, with the effect accumulated on the right *)
let fold_right_map f l e =
  List.fold_right (fun x (l,e) -> let (y,e) = f x e in (y::l,e)) l ([],e)

let on_snd f (x,y) = (x,f y)

let fold_left2_map f e l l' =
  on_snd List.rev @@
  List.fold_left2 (fun (e,l) x x' ->
      let (e,y) = f e x x' in
      (e, y::l)
    ) (e, []) l l'

let fold_right2_map f l l' e =
  List.fold_right2 (fun x x' (l,e) -> let (y,e) = f x x' e in (y::l,e)) l l' ([],e)

let fold_left3_map f e l l' l'' =
  on_snd List.rev @@
  fold_left3 (fun (e,l) x x' x'' -> let (e,y) = f e x x' x'' in (e,y::l)) (e,[]) l l' l''

let fold_left4_map f e l1 l2 l3 l4 =
  on_snd List.rev @@
  fold_left4 (fun (e,l) x1 x2 x3 x4 -> let (e,y) = f e x1 x2 x3 x4 in (e,y::l)) (e,[]) l1 l2 l3 l4

let fold_left5_map f e l1 l2 l3 l4 l5 =
  on_snd List.rev @@
  fold_left5 (fun (e,l) x1 x2 x3 x4 x5 -> let (e,y) = f e x1 x2 x3 x4 x5 in (e,y::l)) (e,[]) l1 l2 l3 l4 l5

(** {6 Splitting} *)

let remove cmp x l =
  List.filter (fun y -> not (cmp x y)) l

let rec remove_first p = function
  | b :: l when p b -> l
  | b :: l -> b :: remove_first p l
  | [] -> raise Not_found

let extract_first p li =
  let rec loop rev_left = function
    | [] -> raise Not_found
    | x :: right ->
       if p x then List.rev_append rev_left right, x
       else loop (x :: rev_left) right
  in
  loop [] li

let insert p v l =
  let rec insrec = function
    | [] -> [v]
    | h :: tl -> if p v h then v :: h :: tl else h :: insrec tl
  in
  insrec l

let find_map_exn f l =
  match find_map f l with
  | Some v -> v
  | None -> raise Not_found

(* FIXME: again, generic hash function *)

let subset l1 l2 =
  let t2 = Hashtbl.create 151 in
  List.iter (fun x -> Hashtbl.add t2 x ()) l2;
  let rec look = function
    | [] -> true
    | x :: ll -> try Hashtbl.find t2 x; look ll with Not_found -> false
  in
  look l1

(** [goto i l] splits [l] into two lists [(l1,l2)] such that
    [(List.rev l1)++l2=l] and [l1] has length [i].  It raises
    [IndexOutOfRange] when [i] is negative or greater than the
    length of [l]. *)
exception IndexOutOfRange
let goto n l =
  let rec goto i acc = function
    | tl when Int.equal i 0 -> (acc, tl)
    | h :: t -> goto (pred i) (h :: acc) t
    | [] -> raise IndexOutOfRange
  in
  goto n [] l

(* [chop i l] splits [l] into two lists [(l1,l2)] such that
   [l1++l2=l] and [l1] has length [i].
   It raises [Failure] when [i] is negative or greater than the length of [l]  *)

let chop n l =
  try let (h,t) = goto n l in (List.rev h,t)
  with IndexOutOfRange -> failwith "List.chop"
    (* spiwack: should raise [IndexOutOfRange] but I'm afraid of missing
       a try/with when replacing the exception. *)

(* [split_when p l] splits [l] into two lists [(l1,a::l2)] such that
    [l1++(a::l2)=l], [p a=true] and [p b = false] for every element [b] of [l1].
    If there is no such [a], then it returns [(l,[])] instead *)
let split_when p =
  let rec split_when_loop x y =
    match y with
    | [] -> (List.rev x,[])
    | (a :: l)  -> if (p a) then (List.rev x,y) else split_when_loop (a :: x) l
  in
  split_when_loop []

let firstn n ol =
  let rec aux acc n l =
    match n, l with
    | 0, [] -> ol
    | 0, _ :: _ -> List.rev acc
    | n, h :: t -> aux (h :: acc) (pred n) t
    | _ -> failwith "firstn"
  in
  aux [] n ol

let sep_first = function
  | [] -> failwith "sep_first"
  | hd :: tl -> (hd, tl)

let rec sep_last = function
  | [] -> failwith "sep_last"
  | hd :: [] -> (hd,[])
  | hd :: tl -> let (l,tl) = sep_last tl in (l,hd :: tl)

(* Drop the last element of a list *)

let rec drop_last = function
  | [] -> failwith "drop_last"
  | hd :: [] -> []
  | hd :: tl -> hd :: drop_last tl

let rec last = function
  | [] -> failwith "List.last"
  | hd :: [] -> hd
  | _ :: tl -> last tl

let lastn n l =
  let len = List.length l in
  let rec aux m l =
    if Int.equal m n then l else aux (m - 1) (List.tl l)
  in
  if len < n then failwith "lastn" else aux len l

let rec skipn n l = match n,l with
  | 0, _ -> l
  | _, [] -> failwith "List.skipn"
  | n, _ :: l -> skipn (pred n) l

let skipn_at_best n l =
  try skipn n l with Failure _ when n >= 0 -> []

(** if [l=p++t] then [drop_prefix p l] is [t] else [l] *)

let drop_prefix cmp p l =
  let rec drop_prefix_rec = function
    | (h1 :: tp, h2 :: tl) when cmp h1 h2 -> drop_prefix_rec (tp,tl)
    | ([], tl) -> tl
    | _ -> l
  in
  drop_prefix_rec (p,l)

let share_tails eq l1 l2 =
  let rec shr_rev acc = function
    | (x1 :: l1, x2 :: l2) when eq x1 x2 -> shr_rev (x1 :: acc) (l1,l2)
    | (l1, l2) -> (List.rev l1, List.rev l2, acc)
  in
  shr_rev [] (List.rev l1, List.rev l2)

(** {6 Association lists} *)

let map_assoc f = map (fun (x,a) -> (x,f a))

let rec assoc_f f a = function
  | (x, e) :: xs -> if f a x then e else assoc_f f a xs
  | [] -> raise Not_found

let rec assoc_f_opt f a = function
  | (x, e) :: xs -> if f a x then Some e else assoc_f_opt f a xs
  | [] -> None

let remove_assoc_f f a l =
  try remove_first (fun (x,_) -> f a x) l with Not_found -> l

let mem_assoc_f f a l = List.exists (fun (x,_) -> f a x) l

(** {6 Operations on lists of tuples} *)

let rec split_loop p q = function
  | [] -> ()
  | (x, y) :: l ->
    let cl = { head = x; tail = [] } in
    let cr = { head = y; tail = [] } in
    p.tail <- cast cl;
    q.tail <- cast cr;
    split_loop cl cr l

let split = function
  | [] -> [], []
  | (x, y) :: l ->
    let cl = { head = x; tail = [] } in
    let cr = { head = y; tail = [] } in
    split_loop cl cr l;
    (cast cl, cast cr)

let rec combine_loop p l1 l2 = match l1, l2 with
  | [], [] -> ()
  | x :: l1, y :: l2 ->
    let c = { head = (x, y); tail = [] } in
    p.tail <- cast c;
    combine_loop c l1 l2
  | _ -> invalid_arg "List.combine"

let combine l1 l2 = match l1, l2 with
  | [], [] -> []
  | x :: l1, y :: l2 ->
    let c = { head = (x, y); tail = [] } in
    combine_loop c l1 l2;
    cast c
  | _ -> invalid_arg "List.combine"

let rec split3_loop p q r = function
  | [] -> ()
  | (x, y, z) :: l ->
    let cp = { head = x; tail = [] } in
    let cq = { head = y; tail = [] } in
    let cr = { head = z; tail = [] } in
    p.tail <- cast cp;
    q.tail <- cast cq;
    r.tail <- cast cr;
    split3_loop cp cq cr l

let split3 = function
  | [] -> [], [], []
  | (x, y, z) :: l ->
    let cp = { head = x; tail = [] } in
    let cq = { head = y; tail = [] } in
    let cr = { head = z; tail = [] } in
    split3_loop cp cq cr l;
    (cast cp, cast cq, cast cr)

(** XXX TODO tailrec *)
let rec split4 = function
  | [] -> ([], [], [], [])
  | (a,b,c,d)::l ->
      let (ra, rb, rc, rd) = split4 l in (a::ra, b::rb, c::rc, d::rd)

let rec combine3_loop p l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> ()
  | x :: l1, y :: l2, z :: l3 ->
    let c = { head = (x, y, z); tail = [] } in
    p.tail <- cast c;
    combine3_loop c l1 l2 l3
  | _ -> invalid_arg "List.combine3"

let combine3 l1 l2 l3 = match l1, l2, l3 with
  | [], [], [] -> []
  | x :: l1, y :: l2, z :: l3 ->
    let c = { head = (x, y, z); tail = [] } in
    combine3_loop c l1 l2 l3;
    cast c
  | _ -> invalid_arg "List.combine3"

(** {6 Operations on lists seen as sets, preserving uniqueness of elements} *)

(** Add an element, preserving uniqueness of elements *)

let add_set cmp x l =
  if mem_f cmp x l then l else x :: l

(** List equality up to permutation (but considering multiple occurrences) *)

let eq_set cmp l1 l2 =
  let rec aux l1 = function
  | [] -> is_empty l1
  | a :: l2 -> aux (remove_first (cmp a) l1) l2
  in
  try aux l1 l2 with Not_found -> false

let rec merge_set cmp l1 l2 = match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
    let c = cmp h1 h2 in
    if Int.equal c 0
    then h1 :: merge_set cmp t1 t2
    else if c <= 0
    then h1 :: merge_set cmp t1 l2
    else h2 :: merge_set cmp l1 t2

let intersect cmp l1 l2 =
  filter (fun x -> mem_f cmp x l2) l1

let union cmp l1 l2 =
  let rec urec = function
    | [] -> l2
    | a :: l -> if mem_f cmp a l2 then urec l else a :: urec l
  in
  urec l1

let subtract cmp l1 l2 =
  if is_empty l2 then l1
  else List.filter (fun x -> not (mem_f cmp x l2)) l1

let unionq l1 l2 = union (==) l1 l2
let subtractq l1 l2 = subtract (==) l1 l2

(** {6 Uniqueness and duplication} *)

(* FIXME: we should avoid relying on the generic hash function,
   just as we'd better avoid Pervasives.compare *)

let distinct l =
  let visited = Hashtbl.create 23 in
  let rec loop = function
    | h :: t ->
        if Hashtbl.mem visited h then false
        else
          begin
            Hashtbl.add visited h h;
            loop t
          end
    | [] -> true
  in
  loop l

let distinct_f cmp l =
  let rec loop = function
    | a :: b :: _ when Int.equal (cmp a b) 0 -> false
    | a :: l -> loop l
    | [] -> true
  in loop (List.sort cmp l)

(* FIXME: again, generic hash function *)

let uniquize_key f l =
  let visited = Hashtbl.create 23 in
  let rec aux acc changed = function
    | h :: t ->
        let x = f h in
        if Hashtbl.mem visited x then aux acc true t else
          begin
            Hashtbl.add visited x x;
            aux (h :: acc) changed t
          end
    | [] -> if changed then List.rev acc else l
  in
  aux [] false l

let uniquize l = uniquize_key (fun x -> x) l

(** [sort_uniquize] might be an alternative to the hashtbl-based
    [uniquize], when the order of the elements is irrelevant *)

let rec uniquize_sorted cmp = function
  | a :: b :: l when Int.equal (cmp a b) 0 -> uniquize_sorted cmp (a :: l)
  | a :: l -> a :: uniquize_sorted cmp l
  | [] -> []

let sort_uniquize cmp l =
  uniquize_sorted cmp (List.sort cmp l)

let min cmp l =
  let rec aux cur = function
    | [] -> cur
    | x :: l -> if cmp x cur < 0 then aux x l else aux cur l
  in
  match l with
  | x :: l -> aux x l
  | [] -> raise Not_found

let rec duplicates cmp = function
  | [] -> []
  | x :: l ->
      let l' = duplicates cmp l in
      if mem_f cmp x l then add_set cmp x l' else l'

(** {6 Cartesian product} *)

(* A generic cartesian product: for any operator (**),
   [cartesian (**) [x1;x2] [y1;y2] = [x1**y1; x1**y2; x2**y1; x2**y1]],
   and so on if there are more elements in the lists. *)

let cartesian op l1 l2 =
  map_append (fun x -> map (op x) l2) l1

(* [cartesians] is an n-ary cartesian product: it iterates
   [cartesian] over a list of lists.  *)

let cartesians op init ll =
  List.fold_right (cartesian op) ll [init]

(* combinations [[a;b];[c;d]] gives [[a;c];[a;d];[b;c];[b;d]] *)

let combinations l =
  cartesians (fun x l -> x :: l) [] l

(* Keep only those products that do not return None *)

let cartesian_filter op l1 l2 =
  map_append (fun x -> map_filter (op x) l2) l1

(* Keep only those products that do not return None *)

let cartesians_filter op init ll =
  List.fold_right (cartesian_filter op) ll [init]

(* Factorize lists of pairs according to the left argument *)
let rec factorize_left cmp = function
  | (a,b) :: l ->
      let al,l' = partition (fun (a',_) -> cmp a a') l in
      (a,(b :: map snd al)) :: factorize_left cmp l'
  | [] -> []

module Smart =
struct

  let rec map f l = match l with
  | [] -> l
  | h :: tl ->
    let h' = f h in
    let tl' = map f tl in
    if h' == h && tl' == tl then l else h' :: tl'

  let rec fold_left_map f e l =
    match l with
    | [] -> e, []
    | h :: tl ->
      let e', h' = f e h in
      let e'', tl' = fold_left_map f e' tl in
      e'', (if h' == h && tl' == tl then l else h' :: tl')

  let rec fold_right_map f l e =
    match l with
    | [] -> [], e
    | h :: tl ->
      let tl', e' = fold_right_map f tl e in
      let h', e'' = f h e' in
      (if h' == h && tl' == tl then l else h' :: tl'), e''

end

module type MonoS = sig
  type elt
  val equal : elt list -> elt list -> bool
  val mem : elt -> elt list -> bool
  val assoc : elt -> (elt * 'a) list -> 'a
  val mem_assoc : elt -> (elt * 'a) list -> bool
  val remove_assoc : elt -> (elt * 'a) list -> (elt * 'a) list
  val mem_assoc_sym : elt -> ('a * elt) list -> bool
end
