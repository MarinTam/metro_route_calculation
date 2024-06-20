#use "global.ml" ;;
(* open Metro
open Global *)
(* open Higashi *)
(* open Kansai *)
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
}
 *) 
type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
} 

(* 二分探索木を表すモジュールのシグネチャ *)
module Tree : sig
  type ('a, 'b) t = Empty
		  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  (* キーが 'a 型、値が 'b 型の木 *)

  val empty : ('a, 'b) t
  (* 使い方：empty *)
  (* 空の木を表す *)

  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方：insert tree key value *)
  (* 木 tree にキー key と値 value を挿入した木を返す *)
  (* キーが既に存在していたら新しい値に置き換える *)

  val search : ('a, 'b) t -> 'a -> 'b
  (* 使い方：search tree key *)
  (* 木 tree の中からキー key に対応する値を探して返す *)
  (* なければ例外 Not_found を起こす *)

  val height : ('a, 'b) t -> int
  (* 使い方：height tree *)
  (* 木 tree の高さを求める *)

  val length : ('a, 'b) t -> int
  (* 使い方：length tree *)
  (* 木 tree のノードの数を求める *)

  (* 二分探索木を表すモジュール *)
end = struct
  (* 二分探索木を表す型 *)
  type ('a, 'b) t = Empty
		  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t
  (* 空の木 *)
  let empty = Empty

  (* 目的：treeにキーがkで値がvを挿入した木をかえす *)
  let rec insert tree k v = match tree with
      Empty -> Node (Empty, k, v, Empty)
    | Node (left, key, value, right) -> 
      if k = key
      then Node (left, k, v, right)
      else if k < key
      then Node (insert left k v, key, value, right)
      else Node (left, key, value, insert right k v)

  let rec search tree k = match tree with
      Empty -> raise Not_found
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
      
(* r1 : total_price : ('a, int) t -> 'a list -> int *)

let rec total_price tree lst =
  let rec search_lst tree1 lst1 =
    match lst1 with
      [] -> 0
    | first :: rest -> Tree.search tree first + search_lst tree1 rest
  in try search_lst tree lst
  with Not_found -> 0

let t1 = Tree.Node (Tree.Empty, "a",20,Tree.Node(Tree.Node(Tree.Empty,"b",40,Tree.Empty),"c",50,Tree.Node(Tree.Empty,"d",90,Tree.Empty))) 
let yasai = ["a";"b";"c"]
let yasai1 = ["a";"b";"c";"tomato"]
let yasai2 = ["a";"b";"c";"d"]


let test1 = total_price t1 yasai = 110
let test2 = total_price t1 yasai1 = 0
let test3 = total_price t1 yasai2 = 200

let rec assoc ekimei_p1 ekimei_kyori_lst =
  match ekimei_kyori_lst with
    [] -> infinity
  | first :: rest -> match first with
      (ekimei_p2, kyori) -> if ekimei_p1 = ekimei_p2 then kyori else assoc ekimei_p1 rest


(* 問２ 問１の木(ekikan_tree)とekikan_t型の駅間を受け取ったら、その情報を挿入した木を返す *)
(* insert_ekikan : (string * string, ((string * string) * float) list) Tree.t ->
  ekikan_t -> (string * string, ((string * string) * float) list) Tree.t *)
let rec insert_ekikan ekikan_tree ekikan =
  match ekikan with
    {kiten = kiten_v; kenk = kenk_v; shuten = shuten_v; kens = kens_v; keiyu = keiyu_v; kyori = kyori_v; jikan = jikan_v} ->
    let rec search_k =  try Tree.search ekikan_tree (kiten_v, kenk_v)
                        with Not_found -> []
  in (let rec search_s = try Tree.search ekikan_tree (shuten_v,kens_v)
                          with Not_found -> []
      in (let inserted_val_s =
        Tree.insert ekikan_tree (shuten_v,kens_v) (((kiten_v,kenk_v),kyori_v) :: search_s)
          in Tree.insert inserted_val_s (kiten_v,kenk_v) (((shuten_v,kens_v),kyori_v) :: search_k)))

let s1 = Tree.Empty
let s2 = Tree.insert s1 ("新大塚","東京") []
let s3 = Tree.insert s2 ("茗荷谷","東京") []
let s4 = Tree.insert s2 ("A","東京") []
let s5 = Tree.insert s2 ("C","東京") []
   

let test1 = insert_ekikan s3
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}
    = Tree.Node (Tree.Empty, ("新大塚", "東京"),
   [(("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),
              [(("新大塚", "東京"), 1.2)], Tree.Empty))
let test2 = insert_ekikan s4
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}
    = Tree.Node (Tree.Node (Tree.Empty, ("A", "東京"), [], Tree.Empty),
   ("新大塚", "東京"), [(("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),
    [(("新大塚", "東京"), 1.2)], Tree.Empty))
let test3 = insert_ekikan s5
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}
    = Tree.Node (Tree.Node (Tree.Empty, ("C", "東京"), [], Tree.Empty),
   ("新大塚", "東京"), [(("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),
    [(("新大塚", "東京"), 1.2)], Tree.Empty))

(* 問2　駅間をひとつではなくリストで受け取り、それを順に挿入した木を返す *)
(* inserts_ekikan : (string * string, ((string * string) * float) list) Tree.t -> ekikan_t list -> (string * string, ((string * string) * float) list) Tree.t *)
let rec inserts_ekikan ekikan_tree ekikan_lst =
  match ekikan_lst with
    [] -> ekikan_tree
  | first :: rest -> let inserted_tree = insert_ekikan ekikan_tree first
                         in inserts_ekikan inserted_tree rest


 let test1 = inserts_ekikan s3
    [{kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2};
     {kiten="C"; kenk="東京"; shuten="D"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=3};
     {kiten="新大塚"; kenk="東京"; shuten="後楽園"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}]
    = Tree.Node
   (Tree.Node
     (Tree.Node (Tree.Empty, ("C", "東京"), [(("D", "東京"), 1.8)],
       Tree.Empty),
     ("D", "東京"), [(("C", "東京"), 1.8)],
     Tree.Node (Tree.Empty, ("後楽園", "東京"),
      [(("新大塚", "東京"), 1.8)], Tree.Empty)),
   ("新大塚", "東京"),
   [(("後楽園", "東京"), 1.8); (("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),
    [(("新大塚", "東京"), 1.2)], Tree.Empty)) 

let test2 = inserts_ekikan s3
    [{kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2};
     {kiten="A"; kenk="東京"; shuten="B"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=3};
     {kiten="C"; kenk="東京"; shuten="後楽園"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}]
    = Tree.Node
   (Tree.Node
     (Tree.Node (Tree.Empty, ("A", "東京"), [(("B", "東京"), 1.8)],
       Tree.Empty),
     ("B", "東京"), [(("A", "東京"), 1.8)],
     Tree.Node
      (Tree.Node (Tree.Empty, ("C", "東京"),
        [(("後楽園", "東京"), 1.8)], Tree.Empty),
      ("後楽園", "東京"), [(("C", "東京"), 1.8)], Tree.Empty)),
   ("新大塚", "東京"), [(("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),
    [(("新大塚", "東京"), 1.2)], Tree.Empty))
    
let test3 = inserts_ekikan s3
    [{kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2};
     {kiten="A"; kenk="東京"; shuten="B"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=3};
     {kiten="新大塚"; kenk="東京"; shuten="後楽園"; kens="東京"; keiyu="丸ノ内線"; kyori=1.8; jikan=2}]
    = Tree.Node(Tree.Node
     (Tree.Node (Tree.Empty, ("A", "東京"), [(("B", "東京"), 1.8)],
       Tree.Empty),("B", "東京"), [(("A", "東京"), 1.8)],
     Tree.Node (Tree.Empty, ("後楽園", "東京"),
      [(("新大塚", "東京"), 1.8)], Tree.Empty)),("新大塚", "東京"),
   [(("後楽園", "東京"), 1.8); (("茗荷谷", "東京"), 1.2)],
   Tree.Node (Tree.Empty, ("茗荷谷", "東京"),[(("新大塚", "東京"), 1.2)], Tree.Empty))

(* 問3 駅名ペアふたつとekikan_tree を受け取って来たら、その２駅間の距離を返す *)
(* get_ekikan_kyori2 : string * string -> string * string -> ekikan_t list -> float *)

let rec get_ekikan_kyori2 eki1 eki2 ekikan_tree =
    let search_eki1 = try Tree.search ekikan_tree eki1
         with Not_found -> []
        in let search_eki2 = try Tree.search ekikan_tree eki2
        with Not_found -> []
        in let rec ret_kyori ekis lst =
             match lst with
               [] -> infinity
             | first :: rest -> match first with
                 (pair_v , kyori) -> if pair_v = ekis then kyori else ret_kyori ekis rest
        in let ret_kyori_eki1 = ret_kyori eki2 search_eki1
        in if ret_kyori_eki1 = infinity then ret_kyori eki1 search_eki2
        else ret_kyori eki2 search_eki1

let s6 = Tree.Node(Tree.Node
     (Tree.Node (Tree.Empty, ("A", "東京"), [(("B", "東京"), 1.8)],
       Tree.Empty),("B", "東京"), [(("A", "東京"), 1.8)],
     Tree.Node (Tree.Empty, ("後楽園", "東京"),
      [(("新大塚", "東京"), 1.8)], Tree.Empty)),("新大塚", "東京"),
   [(("後楽園", "東京"), 1.8); (("茗荷谷", "東京"), 1.2)],
    Tree.Node (Tree.Empty, ("茗荷谷", "東京"),[(("新大塚", "東京"), 1.2)], Tree.Empty))


let test1 = get_ekikan_kyori2 ("新大塚", "東京") ("茗荷谷", "東京") s6 = 1.2
let test2 = get_ekikan_kyori2 ("後楽園", "東京") ("新大塚", "東京") s6 = 1.8
let test3 = get_ekikan_kyori2 ("後楽園", "東京") ("茗荷谷", "東京") s6 = infinity
     
          



let make_initial_eki_list2 ekimei_lst ekimei_pair =
  let make_initial_eki_list1 ekimei =
    match ekimei with
    | {kanji = kanji2_v; kana = kana2_v; romaji = romaji2_v; ken = ken2_v}
      -> (if (kanji2_v, ken2_v)
             = ekimei_pair then {namae = (kanji2_v, ken2_v); saitan_kyori = 0.; temae_list = [ekimei_pair]}
          else {namae = (kanji2_v, ken2_v); saitan_kyori = infinity; temae_list = []})
  in List.map make_initial_eki_list1 ekimei_lst

let rec koushin p v ekikan_tree =
  let koushin1 p q eki_tr =
    match p with
    | {namae = namae_p; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v}
      -> (match q with
          | {namae = namae_q; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v}
            -> (let kyori = get_ekikan_kyori2 namae_p namae_q eki_tr +. saitan_kyori2_v
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

let dijkstra kiten_ekimei shuten_ekimei ekimei_lst ekikan_lst =
  let ekikan_tree = inserts_ekikan Tree.empty ekikan_lst
  in (let kiten_pair = romaji_to_kanji2 kiten_ekimei ekimei_lst
  in (let shuten_pair = romaji_to_kanji2 shuten_ekimei ekimei_lst
  in (let sei = seiretsu2 ekimei_lst (* 駅名リストを名前順に *)
  in (let init_eki_t_lst = make_initial_eki_list2 sei kiten_pair (* 起点を設定したeki_t lstを返す *)
  in (let saitan_lst = dijkstra_main init_eki_t_lst ekikan_tree
  in (let rec pick_eki_t pair lst =
  match lst with
  | [] -> {namae = ("foo", "foo"); saitan_kyori = infinity; temae_list = []}
  | first :: rest -> (match first with
  | {namae = namae2_v; saitan_kyori = saitan_kyori3_v; temae_list = temae_list3_v} -> (if namae2_v = pair then first
  else pick_eki_t pair rest))
      in pick_eki_t shuten_pair saitan_lst))))))

(* time : (unit -> 'a) -> 'a * float *)
let time f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time  = Sys.time () in
  (result, end_time -. start_time)




let test1 = dijkstra "tokyo" "shibuya" global_ekimei_list global_ekikan_list
    = {namae = ("渋谷", "東京"); saitan_kyori = 7.39999999999999947;
   temae_list =
    [("渋谷", "東京"); ("表参道", "東京"); ("乃木坂", "東京");
     ("赤坂", "東京"); ("国会議事堂前", "東京");
     ("霞ヶ関", "東京"); ("銀座", "東京"); ("東京", "東京")]}
      
let test2 = dijkstra "tokyo" "hibiya" global_ekimei_list global_ekikan_list
 = {namae = ("日比谷", "東京"); saitan_kyori = 1.5;
    temae_list = [("日比谷", "東京"); ("銀座", "東京"); ("東京", "東京")]}
let test3 = dijkstra "shinkiba" "tokyo" global_ekimei_list global_ekikan_list
 = {namae = ("東京", "東京"); saitan_kyori = 13.3;
   temae_list =
    [("東京", "東京"); ("銀座", "東京"); ("新橋", "東京");
     ("虎ノ門", "東京"); ("溜池山王", "東京");
     ("永田町", "東京"); ("桜田門", "東京");
     ("有楽町", "東京"); ("銀座一丁目", "東京");
     ("新富町", "東京"); ("月島", "東京"); ("豊洲", "東京");
     ("辰巳", "東京"); ("新木場", "東京")]}

let test4 = time (fun () -> dijkstra "tokyo" "shibuya" global_ekimei_list global_ekikan_list)
let test5 = time (fun () -> dijkstra "tokyo" "hibiya" global_ekimei_list global_ekikan_list )
let test6 = time (fun () -> dijkstra "shinkiba" "tokyo" global_ekimei_list global_ekikan_list)
       

(*
let test7 = time (fun () -> dijkstra "kofu" "okaya" global_ekimei_list global_ekikan_list)
let test8 = time (fun () -> dijkstra "kofu" "anayama" global_ekimei_list global_ekikan_list )
let test9 = time (fun () -> dijkstra "kohu" "hinoharu" global_ekimei_list global_ekikan_list)
            *)           

(*
let test10 = time (fun () -> dijkstra "omiya" "katsura" global_ekimei_list global_ekikan_list)
let test11 = time (fun () -> dijkstra "omiya" "kamikatsura" global_ekimei_list global_ekikan_list )
let test12 = time (fun () -> dijkstra "omiya" "sojiji" global_ekimei_list global_ekikan_list)
             *)         
(*
let test13 = time (fun () -> dijkstra "bibi" "osatsu" global_ekimei_list global_ekikan_list)
let test14 = time (fun () -> dijkstra "bibi" "naeho" global_ekimei_list global_ekikan_list )        
let test15 = time (fun () -> dijkstra "bibi" "kamaya" global_ekimei_list global_ekikan_list)
             *)
             


