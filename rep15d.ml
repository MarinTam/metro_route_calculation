open Global
(* open Higashi *)
(* open Kansai *)
open Heap 
open Tree 
(*
module Tree : sig
  type ('a, 'b) t = Empty
		  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  val empty : ('a, 'b) t 
  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t 
  val search : ('a, 'b) t -> 'a -> 'b
  val height : ('a, 'b) t -> int
  val length : ('a, 'b) t -> int 
end = struct
  type ('a, 'b) t = Empty
		  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  let empty = Empty
  let rec insert tree k v = match tree with
      Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) -> 
      if k = key
      then Node (left, k, v, right)
      else if k < key
      then Node (insert left k v, key, value, right)
      else Node (left, key, value, insert right k v)
  let rec search tree k = match tree with
  | Empty -> raise Not_found
  | Node (left, key, value, right) ->
    if k = key then value
    else if k < key then search left k
    else search right k
  let rec fold f init tree = match tree with
      Empty -> init
    | Node (left, key, value, right) ->
      f (fold f init left) key value (fold f init right)
  let height tree = fold (fun left _ _ right -> 1 + max left right) 0 tree
  let length tree = fold (fun left _ _ right -> left + right + 1) 0 tree
end

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
end 
      *)
  (*
type ekimei_t = {
  kanji   : string; (* 漢字の駅名 *)
  kana    : string; (* 読み *)
  romaji  : string; (* ローマ字 *)
  ken     : string; (* 県名 *)
  shozoku : string; (* 所属路線名 *)
  } 

(* 駅間の情報を格納するレコード型 *)
type ekikan_t = {
  kiten  : string; (* 起点 *)
  kenk   : string; (* 起点の県名 *)
  shuten : string; (* 終点 *)
  kens   : string; (* 終点の県名 *)
  keiyu  : string; (* 経由路線名 *)
  kyori  : float;  (* 距離 *)
  jikan  : int;    (* 所要時間 *)
} *)

let rec assoc ekimei_p1 ekimei_kyori_lst =
  match ekimei_kyori_lst with
    [] -> infinity
  | first :: rest -> match first with
      (ekimei_p2, kyori) -> if ekimei_p1 = ekimei_p2 then kyori else assoc ekimei_p1 rest

let ekimei_l = global_ekimei_list
let ekikan_l = global_ekikan_list
   
(* insert_to_ekilst_heap : (float, (string * string, ((string * string) * float) list)) Heap.t -> ekimei_t list -> ((float, (string * string, ((string * string) * float) list)) Heap.t * (string * string,index_t) Tree.t) *)
let insert_to_ekilst_heap (hp, ind_tr) lst =
  let rec ins_all (hp1, ind_tr1) = function
    | [] -> (hp1, ind_tr1)
    | { kanji = kanji_v; ken = ken_v } :: rest ->
      let (index, new_hp) = Heap.insert hp1 infinity ((kanji_v,ken_v), []) in
      let new_ind_tr = Tree.insert ind_tr1 (kanji_v,ken_v) index in
      ins_all (new_hp, new_ind_tr) rest
  in ins_all (hp, ind_tr) lst

(* 問5 最短路問題　ヒープ *)

(* insert_ekikan : (string * string, ((string * string) * float) list) Tree.t ->
   ekikan_t -> (string * string, ((string * string) * float) list) Tree.t *)
    let rec insert_ekikan ekikan_tree ekikan =
  match ekikan with
  | {kiten = kiten_v; kenk = kenk_v; shuten = shuten_v; kens = kens_v; kyori = kyori_v} ->
    let rec search_k = try Tree.search ekikan_tree (kiten_v, kenk_v)
                      with Not_found -> [] in
    let rec search_s = try Tree.search ekikan_tree (shuten_v, kens_v)
                      with Not_found -> [] in
    let updated_tree1 =
      Tree.insert ekikan_tree (kiten_v, kenk_v) (((shuten_v, kens_v), kyori_v) :: search_k) in
    let updated_tree2 =
      Tree.insert updated_tree1 (shuten_v, kens_v) (((kiten_v, kenk_v), kyori_v) :: search_s) in
    updated_tree2

(* inserts_ekikan : (string * string, ((string * string) * float) list) Tree.t -> ekikan_t list -> (string * string, ((string * string) * float) list) Tree.t *)
let rec inserts_ekikan ekikan_tree ekikan_lst =
  match ekikan_lst with
    [] -> ekikan_tree
  | first :: rest -> let inserted_tree = insert_ekikan ekikan_tree first
    in inserts_ekikan inserted_tree rest

let rec get_ekikan_kyori p q ekikan_tree =
  let lst = Tree.search ekikan_tree p in assoc q lst
  
(* 駅名ペアを入れるとヒープのその駅名ペア部分だけ要素を取り出す *)
let get_eki hp index_tree eki_pair =
  let index = Tree.search index_tree eki_pair
      in Heap.get hp index 

(* ヒープとインデックスツリーを同時に更新する *)
        let koushin_heap_index2 hp index_tree eki_t =
  match eki_t with
  | (value, (eki_pair, info)) ->
      try
        let ind = Tree.search index_tree eki_pair in
        let new_hp = Heap.set hp ind value (eki_pair, info) in
        let new_index_tree = Tree.insert index_tree eki_pair ind in
        (new_hp, new_index_tree)
      with Not_found ->
        let (ind, new_hp) = Heap.insert hp value (eki_pair, info) in
        let new_index_tree = Tree.insert index_tree eki_pair ind in
        (new_hp, new_index_tree)

(* 起点を設定した駅のヒープを返す *)
let init_hp kiten_pair ekimei_lst =
  let length = List.length ekimei_lst in
  let hp = Heap.create (length*10) infinity (("NULL","NULL"),[]) in
  let (hp1,index_tree) = insert_to_ekilst_heap (hp,Tree.empty) ekimei_lst in
  let ret_kiten = (0.,(kiten_pair,[])) in
  koushin_heap_index2 hp1 index_tree ret_kiten
  
(* heapとindex_treeを受け取ってくる *)
let rec koushin_h p hp index_tree ekikan_tree =
  match p with
  | (kaku_saitan, (p_namae1,_)) -> if kaku_saitan = infinity then raise Not_found else
  let rec koushin1 p q =
  match p with
  | (saitan_kyori_p, (p_namae,p_temae_lst)) ->
    match q with
    | (saitan_kyori_q, (q_namae,q_temae_lst)) ->
      let new_kyori = get_ekikan_kyori p_namae q_namae ekikan_tree +. saitan_kyori_p in
      (* デバッグ用に変数の値を表示 *)(*
      Printf.printf "p: (%f, %s, %s), q: (%f, %s, %s), new_kyori: %f\n"
        saitan_kyori_p (fst p_namae) (snd p_namae)
        saitan_kyori_q (fst q_namae) (snd q_namae)
        new_kyori; *)
      if new_kyori = infinity then q
      else if saitan_kyori_q >= new_kyori then
        (new_kyori, (q_namae, (q_namae :: p_temae_lst)))
      else q
in let rec koushin2 hp2 ind_tr q_lst = match q_lst with 
      | [] -> (hp2,ind_tr)
      | (q_pair,pq_kyori) :: rest -> 
        let (new_hp,new_ind_tr) =
          try koushin_heap_index2 hp2 ind_tr (koushin1 p (get_eki hp2 ind_tr q_pair))
             with Not_found -> (hp2,ind_tr)
        in koushin2 new_hp new_ind_tr rest 
  in let setuzoku_eki_lst = Tree.search ekikan_tree p_namae1 
  in koushin2 hp index_tree setuzoku_eki_lst

let split_top_with_index heap index_tree =
  if Heap.length heap = 0 then raise Heap.Empty
  else
    let (min, new_heap) = Heap.split_top heap in 
     (min, new_heap, index_tree)

let rec dijkstra_main_h hp index_tree ekikan_tree =
  let (p, v_hp,index_tree2) = split_top_with_index hp index_tree
  in let (new_hp,ind_tr) = try koushin_h p v_hp index_tree2 ekikan_tree
                                 with Not_found -> (v_hp, index_tree2)
  in let v_lst = try dijkstra_main_h new_hp ind_tr ekikan_tree
       with Heap.Empty -> []
  in p :: v_lst
                       
let rec seiretsu2 lst =
    let rec insert lst2 n =
  match lst2 with
  | [] -> [n]
  | first2 :: rest2 ->
    match first2 with {kanji = kanji1; ken = ken1} ->
    match n with {kanji = kanji2; ken = ken2} ->
       (if kanji1 = kanji2 && ken1 = ken2 then lst2
        else if ken2 = ken1 then (if kanji1 < kanji2 then first2 :: insert rest2 n
  else n :: lst2) else if ken1 > ken2 then n :: lst2
  else first2 :: insert rest2 n)
  in match lst with
  | [] -> []
  | first :: rest -> insert rest first

let rec romaji_to_kanji2 romaji lst =
  match lst with
  | [] -> ("", "")
  | {kanji = kanji3_v; romaji = romaji3_v; ken = ken3_v} :: rest ->
    if romaji3_v = romaji then (kanji3_v, ken3_v) else romaji_to_kanji2 romaji rest

let dijkstra_h ekik ekis =
    let ekikan_tree = inserts_ekikan Tree.empty ekikan_l
    in (let kiten_pair = romaji_to_kanji2 ekik ekimei_l
    in (let shuten_pair = romaji_to_kanji2 ekis ekimei_l
    in (let sei = seiretsu2 ekimei_l (* 駅名リストを名前順に *)              
    in (let (initial_hp,init_index_tree) = init_hp kiten_pair sei (* 起点を設定したhpを返す *)
    in (let saitan_lst = dijkstra_main_h initial_hp init_index_tree ekikan_tree
    in (let rec pick_shuten_eki pair lst = match lst with
          | [] -> raise Not_found
          | first :: rest -> match first with (kyori,(namae,route_lst)) ->
         if namae = pair then first else pick_shuten_eki pair rest
                            in pick_shuten_eki shuten_pair saitan_lst))))))

(* time : (unit -> 'a) -> 'a * float *)
let time f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time  = Sys.time () in
  (result, end_time -. start_time)


let test4h = time (fun () -> dijkstra_h "tokyo" "shibuya")
let test5h = time (fun () -> dijkstra_h "tokyo" "hibiya" )
let test6h = time (fun () -> dijkstra_h "shinkiba" "tokyo")

let test7 = time (fun () -> dijkstra_h "kofu" "okaya")
let test8 = time (fun () -> dijkstra_h "kofu" "anayama" )
let test9 = time (fun () -> dijkstra_h "kohu" "hinoharu")
            
let test10 = time (fun () -> dijkstra_h "omiya" "katsura")
let test11 = time (fun () -> dijkstra_h "omiya" "kamikatsura")
let test12 = time (fun () -> dijkstra_h "omiya" "sojiji" )
                      
let test13 = time (fun () -> dijkstra_h "bibi" "osatsu" )
let test14 = time (fun () -> dijkstra_h "bibi" "naeho")
let test15 = time (fun () -> dijkstra_h "bibi" "kamaya")
             
