open Heap
    (*
module Heap : sig
  (* ヒープを表すモジュールのシグネチャ *)
 (* ヒープの添字の型 *)
  type index_t = int ref

  (* 最小値を求める値が 'a 型でその他の付加情報が 'b 型であるヒープの型 *)
  type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

  exception Full
  (* insert したときにヒープが一杯だと raise される例外 *)

  exception Empty
  (* split_top したときにヒープが空だと raise される例外 *)

  val create : int -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：create size key value *)
  (* ヒープのサイズと 'a 型と 'b 型のダミーの値を受け取ったら *)
  (* 空のヒープを返す *)

  val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
  (* 使い方：insert heap key value *)
  (* ヒープに新しい要素を追加する *)
  (* これ以上、入らないときは Full を raise する *)
  (* ヒープは（破壊的に）書き変わる *)

  val get : ('a, 'b) t -> index_t -> 'a * 'b
  (* 使い方：get heap index *)
  (* ヒープの index 番目の要素を返す *)
  (* index が無効であれば Not_found を raise する *)

  val set : ('a, 'b) t -> index_t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：set heap index key value *)
  (* ヒープの index 番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き変わる *)

  val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
  (* 使い方：split_top heap *)
  (* 最小の値を持つものとそれを取り除いたヒープのペアを返す *)
  (* ヒープが空のときは Empty を raise する *)
  (* ヒープは（破壊的に）書き変わる *)

  val length : ('a, 'b) t -> int
  (* 使い方：length heap *)
  (* ヒープ中のデータの数を返す *)

  val fold_left : ('a -> index_t * 'b * 'c -> 'a) -> 'a -> ('b, 'c) t -> 'a
  (* 使い方：fold_left f init heap *)
  (* init から初めてヒープ中の値全てに順に f を適用しこむ *)

end = struct 
  (* ヒープを表すモジュール *)

  (* ヒープの添字の型。このモジュール内でしか変更はできない *)
  type index_t = int ref

  (* 最小値を求める値が 'a 型でその他の付加情報が 'b 型であるヒープの型 *)
  type ('a, 'b) t = int ref * (index_t * 'a * 'b) array

  (* insert したときにヒープが一杯だと raise される例外 *)
  exception Full

  (* split_top したときにヒープが空だと raise される例外 *)
  exception Empty

  (* index_t 型を持つダミーの値 *)
  let example_index = ref (-1)

  (* 値が example_value と同じ型、付加情報が example_info と同じ型で
     最大 max 個の要素を格納できるヒープを返す *)
  let create max example_value example_info =
    (ref 0, Array.make max (example_index, example_value, example_info))

  (* current_index と parent_index の要素を入れ換える *)
  let swap array current_index parent_index =
    let (index_ref_c, value_c, info_c) as entry_c = array.(current_index) in
    let (index_ref_p, value_p, info_p) as entry_p = array.(parent_index)
    in array.(current_index) <- entry_p;
    array.(parent_index) <- entry_c;
    index_ref_c := parent_index;  (* 入れ換えにともなって index も付け変える *)
    index_ref_p := current_index;
    ()

  (* 下方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
  let rec adjust_child num array current_index =
    if current_index >= num then ()
    else let (_, v, _) = array.(current_index) in
      let child1_index = 2 * current_index + 1 in
      let child2_index = child1_index + 1 in
      if child1_index >= num
      then ()
      else let (_, v1, _) = array.(child1_index) in
	if child2_index >= num
	then if v <= v1
	  then ()
	  else (swap array current_index child1_index;
		adjust_child num array child1_index)
	else let (_, v2, _) = array.(child2_index) in
	  if v <= v1 && v <= v2
	  then ()
	  else if v1 < v2
	  then (swap array current_index child1_index;
		adjust_child num array child1_index)
	  else (swap array current_index child2_index;
		adjust_child num array child2_index)

  (* 上方向に向かってヒープの条件を満たすように要素の入れ換えを行う *)
  let rec adjust_parent array current_index =
    if current_index = 0 then ()
    else let (_, value_c, _) = array.(current_index) in
      let parent_index = (current_index - 1) /2 in
      let (_, value_p, _) = array.(parent_index) in
      if value_c < value_p
      then (swap array current_index parent_index;
	    adjust_parent array parent_index)
      else ()

  (* ヒープに新しい要素を追加する *)
  (* これ以上、入らないときは Full を raise する *)
  (* ヒープは（破壊的に）書き変わる *)
  let insert (num_ref, array) v info =
    if !num_ref >= Array.length array
    then raise Full
    else let index = ref !num_ref in
      array.(!num_ref) <- (index, v, info);
      adjust_parent array !num_ref;
      num_ref := !num_ref + 1;
      (index, (num_ref, array))

  (* ヒープの !index_ref 番目の要素を返す *)
  (* index が無効であれば Not_found を raise する *)
  let get (num_ref, array) index_ref =
    if 0 <= !index_ref && !index_ref < !num_ref
    then let (_, a, b) = array.(!index_ref) in
      (a, b)
    else raise Not_found

  (* ヒープの !index_ref 番目の値を更新したヒープを返す *)
  (* ヒープは（破壊的に）書き変わる *)
  let set (num_ref, array) index_ref v info =
    let (_, v', _) = array.(!index_ref)
    in array.(!index_ref) <- (index_ref, v, info);
    if v < v' then adjust_parent array !index_ref
    else adjust_child !num_ref array !index_ref;
    (num_ref, array)

  (* 最小の値を持つものとそれを取り除いたヒープのペアを返す *)
  (* ヒープが空のときは Empty を raise する *)
  (* ヒープは（破壊的に）書き変わる *)
  let split_top (num_ref, array) =
    if !num_ref = 0 then raise Empty else
      let (index_ref, v, info) = array.(0)
      in num_ref := !num_ref - 1;	(* 要素数をひとつ減らす *)
      swap array 0 !num_ref;
      adjust_child !num_ref array 0;
      index_ref := -1;        (* 取り出した先頭の要素の index_ref は無効にする *)
      ((v, info), (num_ref, array))

  (* ヒープ中のデータの数を返す *)
  let length (num_ref, _) = !num_ref

  (* init から初めてヒープ中の値全てに順に f を適用しこむ *)
  let fold_left f init (num_ref, array) =
    let rec loop i result =
      if i < !num_ref then loop (i + 1) (f result array.(i))
      else result
    in loop 0 init

end *)



(* r1 ヒープと整数のリストを受け取ったらそのヒープに受け取ったリストの中の整数を全て入れたヒープを返す *)
(* insert_to_heap : ('a, string) Heap.t -> 'a list -> ('a, string) Heap.t *)
let insert_to_heap hp lst =
  let rec ins_all hp1 lst1 = match lst1 with
    | [] -> hp1
    | first :: rest ->
        let (ind,new_hp) = Heap.insert hp1 first "info" in
        ins_all new_hp rest
  in ins_all hp lst

let hp1 = Heap.create 10 (-1) "NULL"
let hp2 = Heap.create 10 (-1) "NULL"
let hp3 = Heap.create 10 (-1) "NULL"
    
let test1 = insert_to_heap hp1 [11;12;17;14;13;16;15]
    =  ({contents = 7},
   [|({contents = 0}, 11, "info"); ({contents = 1}, 12, "info");
     ({contents = 2}, 15, "info"); ({contents = 3}, 14, "info");
     ({contents = 4}, 13, "info"); ({contents = 5}, 17, "info");
     ({contents = 6}, 16, "info"); ({contents = -1}, -1, "NULL");
     ({contents = -1}, -1, "NULL"); ({contents = -1}, -1, "NULL")|])
let test2 = insert_to_heap hp2 [20;1;30;5;40;70;80;10]
    = ({contents = 8},
   [|({contents = 0}, 1, "info"); ({contents = 1}, 5, "info");
     ({contents = 2}, 30, "info"); ({contents = 3}, 10, "info");
     ({contents = 4}, 40, "info"); ({contents = 5}, 70, "info");
     ({contents = 6}, 80, "info"); ({contents = 7}, 20, "info");
     ({contents = -1}, -1, "NULL"); ({contents = -1}, -1, "NULL")|])
let test3 = insert_to_heap hp3 [2;1;30;50;11;13;17;4;5]
    = ({contents = 9},
   [|({contents = 0}, 1, "info"); ({contents = 1}, 2, "info");
     ({contents = 2}, 13, "info"); ({contents = 3}, 4, "info");
     ({contents = 4}, 11, "info"); ({contents = 5}, 30, "info");
     ({contents = 6}, 17, "info"); ({contents = 7}, 50, "info");
     ({contents = 8}, 5, "info"); ({contents = -1}, -1, "NULL")|])

(* 問1 受け取ったヒープの中の最小値を順番に取り出しヒープ中の要素を小さい順に並べたリストを返す *)
(* extract_from_heap : ('a, 'b) Heap.t -> ('a * 'b) list *)
let extract_from_heap hp =
  let rec ext_all hp1 =
    try let (min_val, new_hp) = Heap.split_top hp1
      in let rest = ext_all new_hp
      in min_val :: rest
    with Heap.Empty -> []
  in ext_all hp

let test1 = extract_from_heap hp1
    = [(11, "info"); (12, "info"); (13, "info"); (14, "info"); (15, "info");
   (16, "info"); (17, "info")]
let test2 = extract_from_heap hp2
    = [(1, "info"); (5, "info"); (10, "info"); (20, "info"); (30, "info");
   (40, "info"); (70, "info"); (80, "info")]
let test3 = extract_from_heap hp3
    = [(1, "info"); (2, "info"); (4, "info"); (5, "info"); (11, "info");
   (13, "info"); (17, "info"); (30, "info"); (50, "info")]


(* 問2 整数のリストを受け取って来たら、それを小さい順に並べて返す *)
(* heap_sort : int list -> int list *)
let heap_sort lst =
  let length = List.length lst
  in let hp = Heap.create length (-1) "NULL"
  in let lst_hp = insert_to_heap hp lst
  in let hp_lst = extract_from_heap lst_hp
  in let rec pick_num lst = match lst with
      | []-> []
      | (num,str) :: rest -> num :: pick_num rest
  in pick_num hp_lst

let test1 = heap_sort [20;1;30;50;40;70;80;10;60;90;11;12;7;4;13;16;15]
    = [1; 4; 7; 10; 11; 12; 13; 15; 16; 20; 30; 40; 50; 60; 70; 80; 90]
let test2 = heap_sort [2;1;30;50;40;70;80;10;6;9;11;13;17;4;3;16;5]
    = [1; 2; 3; 4; 5; 6; 9; 10; 11; 13; 16; 17; 30; 40; 50; 70; 80]
let test3 = heap_sort [1;2;3;4;5;7;8;10;6;9;11;12;17;14;13;16;15]
    = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17]
