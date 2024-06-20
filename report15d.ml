#use "metro.ml" ;;

(* open Heap *)
(* open Tree *)

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
 
type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
} 

let rec assoc ekimei_p1 ekimei_kyori_lst =
  match ekimei_kyori_lst with
    [] -> infinity
  | first :: rest -> match first with
      (ekimei_p2, kyori) -> if ekimei_p1 = ekimei_p2 then kyori else assoc ekimei_p1 rest

(*
let hp1 = Heap.create 8602 infinity ("NULL",[]) (* global *)
let hp2 = Heap.create 2613 infinity ("NULL",[]) (* higashi *)
let hp3 = Heap.create 822 infinity ("NULL",[])   (* kansai *)
let hp4 = Heap.create 176 infinity ("NULL",[]) (* metro *)
   *)
let hp1 = Heap.create 10 infinity (("NULL","NULL"),[])
    

(* 駅ヒープと起点の駅名ペアを受け取ったら受け取ったリストの中身を全て入れたヒープと索引を返す *)
(* init_eki_heap : (float, string*string list) Heap.t -> string * string -> (float, string*string list) Heap.t *)(*
let init_eki_heap (hp,index_tree) kiten_pair =
  let (index, new_hp) = Heap.insert hp 0. (kiten_pair,[])
  in let new_index_tree = Tree.insert index_tree kiten_pair index
  in (new_hp,new_index_tree)

let test_init_eki_hp = init_eki_heap (hp1,Tree.empty) ("A","b")
                       *)
let ekimei_l = global_ekimei_list
let ekikan_l = global_ekikan_list

(* insert_to_heap : ('a, string) Heap.t -> 'a list -> ('a, string) Heap.t *)
let insert_to_heap hp lst =
  let rec ins_all hp1 lst1 = match lst1 with
    | [] -> hp1
    | first :: rest ->
        let (ind,new_hp) = Heap.insert hp1 first "info" in
        ins_all new_hp rest
  in ins_all hp lst

(* ヒープと駅のリストを受け取ったらそのヒープに受け取ったリストの中身を全て入れたヒープを返す *)
(* それぞれの駅がヒープ中のどこに格納されたかを検索できる木を作成 *)
(* (string,index_t)Tree.t でTree.searchすると駅名に対応する添え字を返す *)
(* insert_to_eki_heap : (float, (string * string, ((string * string) * float) list)) Heap.t -> ekimei_t list -> ((float, (string * string, ((string * string) * float) list)) Heap.t * (string,index_t) Tree.t *)
let insert_eki_heap hp lst =
  let rec ins_all hp1 lst1 = match lst1 with
    | [] -> hp1
    | { kanji = kanji_v; kana = kana_v; romaji = romaji_v; ken = ken_v } :: rest -> 
        let (ind,new_hp) = Heap.insert hp1 infinity ((kanji_v,ken_v),[]) in
        ins_all new_hp rest
  in ins_all hp lst
(*
(* 駅名ペアを入れるとヒープのその駅名ペア部分だけ要素を取り出す *)
let get_eki hp index_tree eki_pair =
  let index = Tree.search index_tree eki_pair
      in Heap.get hp index 
         *)
(* insert_to_eki_heap : (float, (string * string, ((string * string) * float) list)) Heap.t * (string*string, index_t) Tree.t -> ekimei_t -> (float, (string * string, ((string * string) * float) list)) Heap.t * (string * string,index_t) Tree.t) *)

(*
let insert_to_eki_heap (hp,index_tree) ekimei ekikan_tree =
  match ekimei with
  | { kanji = kanji_v; kana = kana_v; romaji = romaji_v; ken = ken_v } ->
    let ekimei_pair = (kanji_v, ken_v) in
    let temaelst = Tree.search ekikan_tree ekimei_pair in
    let saitan_kyori = match temaelst with
    | [] -> (hp,index_tree)
    | (q_pair,pq_kyori) :: rest ->
      let (saitan_p,(p,_)) = get_eki hp index_tree ekimei_pair
    in let (saitan_q,_) = try get_eki hp index_tree q_pair
    with Not_found -> (infinity,_)
    in let saitan_pq = saitan_p +. pq_kyori in
      if saitan_pq > saitan_q then 
   let (index, new_hp) = Heap.insert hp infinity (ekimei_pair, []) in
      let new_ind_tr = Tree.insert ind_tr1 (kanji_v,ken_v) index in
      

 in let koushin1 p q eki_tr =
    (let kyori = get_ekikan_kyori p q eki_tr +. saitan_kyori_p
                in (if kyori = infinity then hp else if saitan_kyori_q >= kyori
                    then koushin_heap hp index_tree q kyori el else hp)))
  in (match temae_lst with
      | [] -> hp
      | first :: rest -> koushin (koushin1 p first ekikan_tree)


                         *)
   
(* insert_to_ekilst_heap : (float, (string * string, ((string * string) * float) list)) Heap.t -> ekimei_t list -> ((float, (string * string, ((string * string) * float) list)) Heap.t * (string * string,index_t) Tree.t) *)
let insert_to_ekilst_heap (hp, ind_tr) lst =
  let rec ins_all (hp1, ind_tr1) = function
    | [] -> (hp1, ind_tr1)
    | { kanji = kanji_v; kana = kana_v; romaji = romaji_v; ken = ken_v } :: rest ->
      let (index, new_hp) = Heap.insert hp1 infinity ((kanji_v,ken_v), []) in
      let new_ind_tr = Tree.insert ind_tr1 (kanji_v,ken_v) index in
      ins_all (new_hp, new_ind_tr) rest
  in ins_all (hp, ind_tr) lst

let (test_hp,test_tr) = insert_to_ekilst_heap (hp1,Tree.Empty) [
{kanji="代々木上原"; kana="よよぎうえはら"; romaji="yoyogiuehara"; ken="東京"; shozoku="千代田線"};
{kanji="代々木公園"; kana="よよぎこうえん"; romaji="yoyogikoen"; ken="東京"; shozoku="千代田線"};
{kanji="明治神宮前"; kana="めいじじんぐうまえ"; romaji="meijijingumae"; ken="東京"; shozoku="千代田線"};
{kanji="表参道"; kana="おもてさんどう"; romaji="omotesando"; ken="東京"; shozoku="千代田線"};
{kanji="乃木坂"; kana="のぎざか"; romaji="nogizaka"; ken="東京"; shozoku="千代田線"};]


(* 起点の駅名ペアを与えると添え字を返す Tree.search index_tree kiten_pair *)
let ind_ex = Tree.search test_tr ("明治神宮前", "東京") 
(* heap中のindex番目の要素を返す Heap.get hp index *)
let (test_get_v,test_get_info) = Heap.get test_hp ind_ex
(* heap中のindex番目の要素を更新 Heap.set hp index value info *)
let test_set = Heap.set test_hp ind_ex 0.0 test_get_info 
               
(* 問1 受け取ったヒープの中の最小値を順番に取り出しヒープ中の要素を小さい順に並べたリストを返す *)
(* extract_from_heap : ('a, 'b) Heap.t -> ('a * 'b) list *)
let extract_from_heap hp =
  let rec ext_all hp1 =
    try let (min_val, new_hp) = Heap.split_top hp1
      in let rest = ext_all new_hp
      in min_val :: rest
    with Heap.Empty -> []
  in ext_all hp
(*
let test1 = extract_from_heap hp1
    = [(11, "info"); (12, "info"); (13, "info"); (14, "info"); (15, "info");
   (16, "info"); (17, "info")] *)


(* 問5 最短路問題　ヒープ *)

(* insert_ekikan : (string * string, ((string * string) * float) list) Tree.t ->
  ekikan_t -> (string * string, ((string * string) * float) list) Tree.t *)
let rec insert_ekikan ekikan_tree ekikan =
  match ekikan with
    {kiten = kiten_v; kenk = kenk_v; shuten = shuten_v; kens = kens_v; kyori = kyori_v} ->
    let rec search_k =  try Tree.search ekikan_tree (kiten_v, kenk_v)
                        with Not_found -> []
  in (let rec search_s = try Tree.search ekikan_tree (shuten_v,kens_v)
                          with Not_found -> []
      in (let inserted_val_s =
        Tree.insert ekikan_tree (shuten_v,kens_v) (((kiten_v,kenk_v),kyori_v) :: search_s)
          in Tree.insert inserted_val_s (kiten_v,kenk_v) (((shuten_v,kens_v),kyori_v) :: search_k)))
(* inserts_ekikan : (string * string, ((string * string) * float) list) Tree.t -> ekikan_t list -> (string * string, ((string * string) * float) list) Tree.t *)
let rec inserts_ekikan ekikan_tree ekikan_lst =
  match ekikan_lst with
    [] -> ekikan_tree
  | first :: rest -> let inserted_tree = insert_ekikan ekikan_tree first
    in inserts_ekikan inserted_tree rest

let rec get_ekikan_kyori p q ekikan_tree =
  let lst = Tree.search ekikan_tree p in
  assoc q lst
  
let make_initial_eki_list2 ekimei_lst ekimei_pair =
  let make_initial_eki_list1 ekimei =
    match ekimei with
    | { kanji = kanji2_v; kana = kana2_v; romaji = romaji2_v; ken = ken2_v }
      -> if (kanji2_v, ken2_v) = ekimei_pair then
        { namae = (kanji2_v, ken2_v); saitan_kyori = 0.; temae_list = [ekimei_pair] }
      else { namae = (kanji2_v, ken2_v);
          saitan_kyori = infinity;
          temae_list = []}
  in List.map make_initial_eki_list1 ekimei_lst


let test_ekikan_tr = inserts_ekikan Tree.empty
    []
    (*{kiten="代々木上原"; kenk="東京"; shuten="代々木公園"; kens="東京";keiyu="千代田線"; kyori=1.0; jikan=2};
{kiten="代々木公園"; kenk="東京"; shuten="明治神宮前"; kens="東京"; keiyu="千代田線"; kyori=1.2; jikan=2};
{kiten="明治神宮前"; kenk="東京"; shuten="表参道"; kens="東京"; keiyu="千代田線"; kyori=0.9; jikan=2};
{kiten="表参道"; kenk="東京"; shuten="乃木坂"; kens="東京"; keiyu="千代田線"; kyori=1.4; jikan=3};
{kiten="乃木坂"; kenk="東京"; shuten="赤坂"; kens="東京"; keiyu="千代田線"; kyori=1.1; jikan=2};
      {kiten="赤坂"; kenk="東京"; shuten="国会議事堂前"; kens="東京"; keiyu="千代田線"; kyori=0.8; jikan=1};] 
(* pに接続する駅のみをリスト化 Tree.search ekikan_tree p *)
let test_setuzoku_eki_lst = Tree.search test_ekikan_tr ("赤坂","東京")
                            *)

(* 駅名ペアを入れるとヒープのその駅名ペア部分だけ要素を取り出す *)
let get_eki hp index_tree eki_pair =
  let index = Tree.search index_tree eki_pair
      in Heap.get hp index 
(* koushin_heap : 駅名ペアと更新内容を入れるとヒープのその駅名ペア部分だけ更新する *)
(* koushin_heap_index : ('a, 'b) Heap.t ->
  ('c, Heap.index_t) Tree.t -> 'c -> 'a -> 'b -> (('a, 'b) Heap.t * ('c, Heap.index_t) Tree.t) *)
let koushin_heap hp index_tree eki_pair value info =
  let index = Tree.search index_tree eki_pair
  in Heap.set hp index value info

(* ヒープとインデックスツリーを同時に更新する *)
let koushin_heap_index hp index_tree eki_pair value info =
  let (_, new_hp) = Heap.insert hp value info in
  let index = Tree.search index_tree eki_pair in
  let new_index_tree = Tree.insert index_tree eki_pair index in
  (new_hp, new_index_tree)

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
  let hp = Heap.create (length*2) infinity (("NULL","NULL"),[]) in
  let (hp1,index_tree) = insert_to_ekilst_heap (hp,Tree.empty) ekimei_lst in
  let ret_kiten = (0.,(kiten_pair,[])) in
  koushin_heap_index2 hp1 index_tree ret_kiten

let split_top_with_index heap index_tree =
  if Heap.length heap = 0 then raise Heap.Empty
  else
    let (min, new_heap) = Heap.split_top heap in (*
    match min with (_,(p_namae,_)) ->
      let index = try Tree.search index_tree p_namae with Not_found -> ref (-1) in
      (* 最小値のインデックス取得 *)
      let index_tree2 = Tree.insert index_tree p_namae index in *)
    (min, new_heap, index_tree)

(*
let test_get_eki = get_eki test_hp test_tr ("乃木坂","東京") 
let test_koushin_heap = koushin_heap_index test_hp test_tr ("乃木坂","東京") 2.2 (("",""),[]) *)
let test_koushin_heap2 = koushin_heap_index2 test_hp test_tr (1.4,(("乃木坂","東京") ,[("","")]))    
let test_koushin_heap22 = koushin_heap_index2 test_hp test_tr (1.2,(("表","愛でしょ") ,[("","")]))    
let test_bunri = split_top_with_index test_hp test_tr
let test_bunri2 = split_top_with_index test_hp test_tr
let test_bunri3 = split_top_with_index test_hp test_tr
   
(* heapとindex_treeを受け取ってくる *)
let rec koushin_h p hp index_tree ekikan_tree =
  match p with
  | (_, (p_namae1,_)) ->
  let koushin1 p q = (* qの内容更新してqの内容を返す *)
       match p with
  | (saitan_kyori_p, (p_namae,p_temae_lst)) ->
       match q with
  | (saitan_kyori_q, (q_namae,q_temae_lst)) ->
    (let new_kyori = get_ekikan_kyori p_namae q_namae ekikan_tree +. saitan_kyori_p
                in (if new_kyori = infinity then q else if saitan_kyori_q >= new_kyori
                    then (new_kyori,(q_namae,(q_namae :: p_temae_lst))) else q))
  in let rec koushin2 p2 hp2 ind_tr q_lst = match q_lst with 
      | [] -> (hp2,ind_tr)
      | (q_pair,pq_kyori) :: rest ->
        let q = get_eki hp2 ind_tr q_pair
        in let (new_hp,new_ind_tr) = koushin_heap_index2 hp2 ind_tr (koushin1 p q)
        in koushin2 p2 new_hp new_ind_tr rest
  in let setuzoku_eki_lst = Tree.search ekikan_tree p_namae1 
          in koushin2 p hp index_tree setuzoku_eki_lst 

let rec koushin p v ekikan_tree =
  let koushin1 p q eki_tr =
    match p with
    | {namae = namae_p; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v}
      -> (match q with
          | {namae = namae_q; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v}
            -> (let kyori = get_ekikan_kyori namae_p namae_q eki_tr +. saitan_kyori2_v
                in (if kyori = infinity then q else if saitan_kyori_v >= kyori
                    then {namae = namae_q; saitan_kyori = kyori;
                          temae_list = namae_q :: temae_list2_v} else q)))
   in (match v with
      | [] -> []
      | first :: rest -> koushin1 p first ekikan_tree :: koushin p rest ekikan_tree)

let saitan_wo_bunri2 eki_t_lst =
  let pick_saitan2 eki_t1 eki_t2 =
    match eki_t2 with
    | {namae = namae8_v; saitan_kyori = saitan_kyori8_v; temae_list = temae_list8_v} -> (match eki_t1 with
        | {namae = namae7_v; saitan_kyori = saitan_kyori7_v; temae_list = temae_list7_v} -> (if saitan_kyori7_v <= saitan_kyori8_v then eki_t1 else eki_t2))
  in (let pick_saitan1 eki_t_lst2 =
        List.fold_right pick_saitan2 eki_t_lst2 {namae = ("", ""); saitan_kyori = infinity; temae_list = []}
      in (let saitan_wo_bunri1 eki_t =
            eki_t <> pick_saitan1 eki_t_lst
          in (pick_saitan1 eki_t_lst, List.filter saitan_wo_bunri1 eki_t_lst)))


let rec dijkstra_main_h hp index_tree ekikan_tree =
  let (p, v_hp) = Heap.split_top hp
  in let (new_hp,ind_tr) = koushin_h p hp index_tree ekikan_tree
  in let v_lst = try dijkstra_main_h new_hp ind_tr ekikan_tree
       with Heap.Empty -> []
  in p :: v_lst
     
                        
let rec dijkstra_main eki_t_lst ekikan_tree =
  match eki_t_lst with
  | [] -> []
  | first :: rest -> (match saitan_wo_bunri2 eki_t_lst with
      | (p, v) -> (match v with
          | [] -> [p]
          | first1 :: rest2 -> p :: dijkstra_main (koushin p v ekikan_tree) ekikan_tree))
let rec seiretsu2 lst =
  match lst with
  | [] -> []
  | first :: rest -> (let rec insert lst n =
  match lst with
  | [] -> [n]
  | {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: rest4 -> (match n with
  | {kanji = kanji2_v; kana = kana2_v; romaji = romaji2_v; ken = ken2_v; shozoku = shozoku2_v} -> (if kanji1 = kanji2_v && ken1 = ken2_v then lst
  else if ken2_v = ken1 then (if kanji1 < kanji2_v then {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: insert rest4 n
  else n :: lst)
  else if ken1 > ken2_v then n :: lst
  else {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: insert rest4 n))
  in (let rec sort lst =
  match lst with
  | [] -> []
  | first5 :: rest5 -> insert (sort rest5) first5
      in sort lst))
let rec romaji_to_kanji2 romaji lst =
  match lst with
  | [] -> ("", "")
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest4 -> (if romaji3_v = romaji then (kanji3_v, ken3_v)   else romaji_to_kanji2 romaji rest4)




let dijkstra_h ekik ekis =
    let ekikan_tree = inserts_ekikan Tree.empty ekikan_l
    in (let kiten_pair = romaji_to_kanji2 ekik ekimei_l
    in (let shuten_pair = romaji_to_kanji2 ekis ekimei_l
    in (let sei = seiretsu2 ekimei_l (* 駅名リストを名前順に *)              
    in (let (initial_hp,init_index_tree) = init_hp kiten_pair sei (* 起点を設定したhpを返す *)
    in (let saitan_lst = dijkstra_main_h initial_hp init_index_tree ekikan_tree
    in (let rec pick_shuten_eki pair lst = match lst with
       [] -> raise Not_found
     | first :: rest -> match first with (kyori,(namae,route_lst)) ->
         if namae = pair then first else pick_shuten_eki pair rest
                            in pick_shuten_eki shuten_pair saitan_lst))))))

       



(* 起点の駅名（ローマ字）と終点の駅名（ローマ字）を受け取る *)
let dijkstra ekik ekis =
    let ekikan_tree = inserts_ekikan Tree.empty ekikan_l
    in (let kiten_pair = romaji_to_kanji2 ekik ekimei_l
    in (let shuten_pair = romaji_to_kanji2 ekis ekimei_l
    in (let sei = seiretsu2 ekimei_l (* 駅名リストを名前順に *)              
    in (let init_hp = make_initial_eki_list2 sei kiten_pair (* 起点を設定したeki_t lstを返す *)
    in (let saitan_lst = dijkstra_main init_hp ekikan_tree
    in (let rec pick_eki_t pair lst = match lst with
       [] -> {namae = ("", ""); saitan_kyori = infinity; temae_list = []}
     | first :: rest -> (match first with
              | {namae = namae2_v} ->
                (if namae2_v = pair then first else pick_eki_t pair rest))
                            in pick_eki_t shuten_pair saitan_lst))))))

(* time : (unit -> 'a) -> 'a * float *)
let time f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time  = Sys.time () in
  (result, end_time -. start_time)






let test4 = time (fun () -> dijkstra "tokyo" "shibuya")
let test5 = time (fun () -> dijkstra "tokyo" "hibiya" )
let test6 = time (fun () -> dijkstra "shinkiba" "tokyo")
                   
let test4h = time (fun () -> dijkstra_h "tokyo" "shibuya")
let test5h = time (fun () -> dijkstra_h "tokyo" "hibiya" )
let test6h = time (fun () -> dijkstra_h "shinkiba" "tokyo")
                   

(*
let test7 = time (fun () -> dijkstra "kofu" "okaya")
let test8 = time (fun () -> dijkstra "kofu" "anayama" )
let test9 = time (fun () -> dijkstra "kohu" "hinoharu")
            
            *)
(*
let test10 = time (fun () -> dijkstra "omiya" "katsura")
let test11 = time (fun () -> dijkstra "omiya" "kamikatsura")
let test12 = time (fun () -> dijkstra "omiya" "sojiji" )
             *)         
(*
let test13 = time (fun () -> dijkstra "bibi" "osatsu" )
let test14 = time (fun () -> dijkstra "bibi" "naeho")
let test15 = time (fun () -> dijkstra "bibi" "kamaya")
             *)
