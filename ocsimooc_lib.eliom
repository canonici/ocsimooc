[%%shared
module Array = struct

  include Array

  let for_alli f a =
    let n = Array.length a in
    let rec g i = i >= n || (f i a.(i) && g (i + 1)) in
    g 0

  let find f a =
    let n = Array.length a in
    let rec g i =
      if i >= n then
        None
      else if f a.(i) then
        Some i
      else
        g (i + 1)
    in
    g 0

end

module List = struct

  include List

  let rec init ?acc:(acc = []) f n =
    assert (n >= 0);
    if n <= 0 then
      acc
    else
      let acc = f (n - 1) :: acc in
      init ~acc f (n - 1)

end

module Opt = struct

  let iter f = function
    | Some x ->
      f x
    | None ->
      ()

end

module Content = struct
  module D = Eliom_content.Html.D

  let (!?) x = [D.li [x]] ;;
  let (|?) y x = D.li [x] :: y ;;

  let (!.) x = !? (D.pcdata x)
  let (|.) y x = y |? D.pcdata x

  let (!+) x = !? (D.ul (List.rev x))
  let (|+) y x = y |? D.ul (List.rev x)

  let ul x = D.ul (List.rev x)

end

module BSTree = struct

  module type ITree = sig

    type t
    type key
    type value

    val empty : t

    val insert : t -> (key * value) -> t

    val of_list : (key * value) list -> t

    val search : t -> key -> value option

  end

  module type S = sig
    type key
    type value
    val compare : key -> key -> int
  end

  module Make ( T : S ) : ITree
    with type key   := T.key
    and  type value := T.value
      =
  struct

    type key = T.key
    type value = T.value

    type 'a h = int * 'a
    type ('a, 'b) tree =
    | Node of ('a * 'b) * ('a, 'b) tree h * ('a, 'b) tree h
    | Empty
    type t = (T.key, T.value) tree h

    let empty = 0, Empty

    let htree_of_tree =
      let max h h' = if h > h' then h else h' in
      let new_height = function
	| Node ((_,_),(lh,_),(rh,_)) -> max lh rh + 1
	| _ -> 0
      in
      fun t -> new_height t, t

    let rec rotate_left t = match t with
      | h, Node ((k,v), lt, (rh, Node ((rk,rv), rlt, rrt))) ->
	let lt' = Node ((k,v), lt, rlt) |> htree_of_tree in
	Node ((rk,rv), rotate_left lt', rrt) |> htree_of_tree
      | _ -> t

    let rec rotate_right t = match t with
      | h, Node ((k,v), (lh, Node ((lk,lv), llt, lrt)), rt) ->
	let rt' = Node ((k,v), lrt, rt) |> htree_of_tree in
	Node ((lk,lv), llt, rotate_right rt') |> htree_of_tree
      | _ -> t

    let rec balance t = match t with
      | h, Node ((k,v), (lh,_), (rh,_)) ->
	if lh - rh > 1 then
	  balance @@ rotate_right t
	else if rh - lh > 1 then
	  balance @@ rotate_left t
	else
	  t
      | _ -> t

    let insert tree (key, value) =
      let rec aux fr = function
	| _, Node ((k, v), lt, ((rh, _) as rt)) when T.compare key k < 0 ->
	  aux (fun (hn, n) -> fr @@
	    let nn = Node ((k, v), (hn, n), rt) |> htree_of_tree in
	    balance nn
	  ) lt
	| _, Node ((k, v), ((lh, _) as lt), rt) when T.compare k key < 0 ->
	  aux (fun (hn, n) -> fr @@
	    let nn = Node ((k, v), lt, (hn, n)) |> htree_of_tree in
	    balance nn
	  ) rt
	| h, Node ((k, v), lt, rt) ->
	  fr @@ (h, Node ((key, value), lt, rt))
	| _, Empty ->
	  fr @@ (1, Node ((key, value), empty, empty))
      in
      aux (fun x -> x) tree

    let of_list l = List.fold_left (fun t e -> insert t e) empty l

    let search t k =
      let rec aux = function
	| _, Node ((k',v'), _, _) when T.compare k' k = 0 ->
	  Some v'
	| _, Node ((k',v'), lt, rt) ->
	  aux @@ if T.compare k' k < 0 then rt else lt
	| _ ->
	  None
      in
      aux t

  end
end
]
