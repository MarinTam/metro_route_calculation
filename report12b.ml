
(* open Metro *)
   open Global 
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


module RedBlack : sig
  (* 問4 *)
  type color_t = Red | Black
  type ('a, 'b, 'c) t = Empty
                      | Node of ('a, 'b ,'c) t * 'a * 'b * color_t * ('a, 'b ,'c) t

  val empty : ('a, 'b, 'c) t
  (* 使い方：empty *)
  (* 空の木を表す *)

  val balance : ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (* 使い方:balance rb_tree *)
  (* 赤黒木を整理する *)


  val insert : ('a, 'b, 'c) t -> 'a -> 'b -> ('a, 'b, 'c) t
  (* 使い方：insert rb_tree key value *)
  (* 赤黒木 rb_tree にキー key と値 value を挿入した木を返す *)
  (* キーが既に存在していたら新しい値に置き換える *)


  val search : ('a, 'b, 'c) t -> 'a -> 'b
  (* 使い方：search rb_tree key *)
  (* 赤黒木　rb_tree の中からキー key に対応する値を探して返す *)
  (* なければ例外 Not_found を起こす *)

  val height : ('a, 'b, 'c) t -> int
  (* 使い方：height tree *)
  (* 木 tree の高さを求める *)

  val length : ('a, 'b, 'c) t -> int
  (* 使い方：length tree *)
  (* 木 tree のノードの数を求める *)

  (* 二分探索木を表すモジュール *)
end = struct
  (* 二分探索木を表す型 *)
  type color_t = Red | Black                   
  type ('a, 'b, 'c) t = Empty
		      | Node of ('a, 'b, 'c) t * 'a * 'b * color_t * ('a, 'b, 'c) t

  (* 空の木 *)
  let empty = Empty

  (* 問5 教科書227に従い赤黒木を再構成する *)
  (* balance : ('a, 'b, 'c) t -> ('a, 'b, 'c) t *)

  let rec balance rb_tr =
    match rb_tr with
      Empty -> Empty
    | Node(Node(Node(a,xk,xv,Red,b),yk,yv,Red,c),zk,zv,Black,d)
    | Node(Node(a,xk,xv,Red,Node(b,yk,yv,Red,c)),zk,zv,Black,d)
    | Node(a,xk,xv,Black,Node(Node(b,yk,yv,Red,c),zk,zv,Red,d))
    | Node(a,xk,xv,Black,Node(b,yk,yv,Red,Node(c,zk,zv,Red,d)))
      -> Node(Node(a,xk,xv,Black,b),yk,yv,Red,Node(c,zk,zv,Black,d))
    | Node(l,k,v,col,r) -> Node(balance l,k,v,col,balance r)

    (*
  let rec balance rb_tr = match rb_tr with
      Empty -> Empty
    | Node(l,k,v,col,r) ->
      if col = Black
      then (match l with
            Empty -> (match r with
                Empty -> rb_tr
              | Node(rl,rk,rv,rcol,rr) ->
                if rcol = Red
                then (match rl with
                      Empty -> (match rr with
                          Empty -> rb_tr
                        | Node(rrl,rrk,rrv,rrcol,rrr) -> if rrcol = Red
                          then Node(Node(l,k,v,col,rl) ,rk,rv,rcol,Node(rrl,rrk,rrv,Black,rrr))
                          else Node(l,k,v,col,Node(rl,rk,rv,rcol,balance rr)))
                    | Node(rll,rlk,rlv,rlcol,rlr) -> if rlcol = Red
                      then Node(Node(l,k,v,col,rll) ,rlk,rlv,rlcol,Node(rlr,rk,rv,Black,rr))
                      else Node(l,k,v,col,Node(balance rl,rk,rv,rcol,rr)))
                else Node(l,k,v,col,balance r))
          | Node(ll,lk,lv,lcol,lr) ->
            (if lcol = Red then (match ll with
                   Empty -> (match lr with
                       Empty -> rb_tr
                     | Node(lrl,lrk,lrv,lrcol,lrr) ->
                       if lrcol = Red
                       then Node(Node(ll,lk,lv,Black,lrl),lrk,lrv,lrcol,Node(lrr,k,v,col,r))
                       else Node(Node(ll,lk,lv,lcol,balance lr),k,v,col,r))
                 | Node(lll,llk,llv,llcol,llr) ->
                   if llcol = Red
                   then Node(Node(lll,llk,llv,Black,llr),lk,lv,lcol,Node(lr,k,v,col,r))
                   else Node(Node(balance ll, lk,lv,lcol,lr),k,v,col,r))
             else Node(balance l, k,v,col,r)))
      else Node(balance l,k,v,col, balance r)
           *)

  (* 問6 赤黒木とキーと値を受け取ったら、それを挿入した赤黒木を返す *)
  (* insert : ('a, 'b, 'c) t -> 'a -> 'b -> ('a, 'b, 'c) t *)
  let rec insert rb_tr k v =
    let ins2 tr3 k3 v3 =
    let rec ins1 tr k1 v1 =
      match tr with
        Empty -> Node(Empty,k1,v1,Red,Empty)
      | Node(l,key,value,col,r) -> 
        if k1 = key
          then Node(l,k1,v1,col,r)
          else if k1 < key
          then Node(ins1 l k1 v1, key,value,col,r)
          else Node(l,key,value,col,ins1 r k1 v1)
    in match tr3 with
      Empty -> Node(Empty,k3,v3,Red,Empty)
    | Node(l,key,value,col,r) -> if col = Red then ins1 tr3 k3 v3 else ins1 (balance tr3) k3 v3 
    in match ins2 rb_tr k v with
      Empty -> Empty
    | Node(l,k2,v2,c,r) -> Node(l,k2,v2,Black,r)

  
  (* 問7 赤黒木とキーを受け取ったら、そのキーを持つノードに保存されている値を返す *)
  (* search : ('a, 'b, 'c) t -> 'a -> 'b *)
  let rec search rb_tr k = match rb_tr with
      Empty -> raise Not_found
    | Node(l,key,value,col,r) ->
      if k = key then value
      else if k < key then search l k
      else search r k

  let rec fold f init tree = match tree with
      Empty -> init
    | Node (left, key, value, color, right) ->
      f (fold f init left) key value (fold f init right)

  let height tree = fold (fun left _ _ right -> 1 + max left right) 0 tree

  let length tree = fold (fun left _ _ right -> left + right + 1) 0 tree



end

let t1 = RedBlack.Node(RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty)
let t2 = RedBlack.insert t1 "b" 2
let t3 = RedBlack.insert t2 "i" 9
let t4 = RedBlack.insert t3 "g" 7   
let t5 = RedBlack.insert t4 "k" 11  
let t6 = RedBlack.insert t5 "e" 5

let test1 = RedBlack.balance t3
            = RedBlack.Node
              (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
               "b", 2, RedBlack.Red,
               RedBlack.Node (RedBlack.Empty, "i", 9, RedBlack.Black, RedBlack.Empty))
let test2 = RedBlack.balance t5
            = RedBlack.Node
   (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
   "b", 2, RedBlack.Black,
   RedBlack.Node
    (RedBlack.Node (RedBlack.Empty, "g", 7, RedBlack.Red, RedBlack.Empty),
    "i", 9, RedBlack.Black,
    RedBlack.Node (RedBlack.Empty, "k", 11, RedBlack.Red, RedBlack.Empty)))
let test3 = RedBlack.balance t6
           =RedBlack.Node
   (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
   "b", 2, RedBlack.Black,
   RedBlack.Node
    (RedBlack.Node (RedBlack.Empty, "e", 5, RedBlack.Black, RedBlack.Empty),
    "g", 7, RedBlack.Red,
    RedBlack.Node (RedBlack.Empty, "i", 9, RedBlack.Black,
     RedBlack.Node (RedBlack.Empty, "k", 11, RedBlack.Red, RedBlack.Empty))))

let test4 = RedBlack.insert t4 "j" 10
           = RedBlack.Node
   (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
   "b", 2, RedBlack.Black,
   RedBlack.Node
    (RedBlack.Node (RedBlack.Empty, "g", 7, RedBlack.Red, RedBlack.Empty),
    "i", 9, RedBlack.Black,
    RedBlack.Node (RedBlack.Empty, "j", 10, RedBlack.Red, RedBlack.Empty)))
let test5 = RedBlack.insert t5 "n" 14
           = RedBlack.Node
   (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
   "b", 2, RedBlack.Black,
   RedBlack.Node
    (RedBlack.Node (RedBlack.Empty, "g", 7, RedBlack.Red, RedBlack.Empty),
    "i", 9, RedBlack.Black,
    RedBlack.Node (RedBlack.Empty, "k", 11, RedBlack.Red,
     RedBlack.Node (RedBlack.Empty, "n", 14, RedBlack.Red, RedBlack.Empty))))
let test6 = RedBlack.insert t6 "f" 7
           = RedBlack.Node
   (RedBlack.Node (RedBlack.Empty, "a", 1, RedBlack.Black, RedBlack.Empty),
   "b", 2, RedBlack.Black,
   RedBlack.Node
    (RedBlack.Node (RedBlack.Empty, "e", 5, RedBlack.Black,
      RedBlack.Node (RedBlack.Empty, "f", 7, RedBlack.Red, RedBlack.Empty)),
    "g", 7, RedBlack.Red,
    RedBlack.Node (RedBlack.Empty, "i", 9, RedBlack.Black,
     RedBlack.Node (RedBlack.Empty, "k", 11, RedBlack.Red, RedBlack.Empty))))

let test7 = RedBlack.search t6 "g" = 7
let test8 = RedBlack.search t6 "i" = 9
let test9 = RedBlack.search t6 "e" = 5



(* 問9　駅間をひとつではなくリストで受け取り、それを順に挿入した木を返す *)
let rec insert_ekikan ekikan_tree ekikan =
  let ins_ekikan = 
    match ekikan with
      {kiten = kiten_v; kenk = kenk_v; shuten = shuten_v; kens = kens_v; keiyu = keiyu_v; kyori = kyori_v; jikan = jikan_v} ->
      let rec search_k =  try RedBlack.search ekikan_tree (kiten_v, kenk_v)
        with Not_found -> []
      in (let rec search_s = try RedBlack.search ekikan_tree (shuten_v,kens_v)
            with Not_found -> []
          in (let inserted_val_s =
                RedBlack.insert ekikan_tree (shuten_v,kens_v) (((kiten_v,kenk_v),kyori_v) :: search_s)
              in RedBlack.insert inserted_val_s (kiten_v,kenk_v) (((shuten_v,kens_v),kyori_v) :: search_k)))
  in RedBlack.balance ins_ekikan

let s1 = RedBlack.Empty
let s2 = RedBlack.insert s1 ("新大塚","東京") []
let s3 = RedBlack.insert s2 ("茗荷谷","東京") []
let s4 = RedBlack.insert s2 ("A","東京") []
let s5 = RedBlack.insert s2 ("C","東京") [] 

let test1 = insert_ekikan s3
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}

let test2 = insert_ekikan s4
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}

let test3 = insert_ekikan s5
    {kiten="新大塚"; kenk="東京"; shuten="茗荷谷"; kens="東京"; keiyu="丸ノ内線"; kyori=1.2; jikan=2}


(* inserts_ekikan : (string * string, ((string * string) * float) list) RedBlack.t -> ekikan_t list -> (string * string, ((string * string) * float) list) RedBlack.t *)
let rec inserts_ekikan ekikan_tree ekikan_lst =
  match ekikan_lst with
    [] -> ekikan_tree
  | first :: rest -> let inserted_tree = insert_ekikan ekikan_tree first
    in inserts_ekikan inserted_tree rest



(* open Global    
   open Report12b   
   let ekikan_tree = Report12b.inserts_ekikan RedBlack.Empty global_ekikan_list
   let height_of_tree = RedBlack.height ekikan_tree ;;
   
val height_of_tree : int = 18
*)

(*
open Global
open Report12a
let ekikan_tree = Report12a.inserts_ekikan Tree.Empty global_ekikan_list
let height_of_tree = Tree.height ekikan_tree ;;  

val height_of_tree : int = 32
*)


let rec get_ekikan_kyori2 eki1 eki2 ekikan_tree =
    let search_eki1 = try RedBlack.search ekikan_tree eki1
         with Not_found -> []
        in let search_eki2 = try RedBlack.search ekikan_tree eki2
        with Not_found -> []
        in let rec ret_kyori ekis lst =
             match lst with
               [] -> infinity
             | first :: rest -> match first with
                 (pair_v , kyori) -> if pair_v = ekis then kyori else ret_kyori ekis rest
        in let ret_kyori_eki1 = ret_kyori eki2 search_eki1
        in if ret_kyori_eki1 = infinity then ret_kyori eki1 search_eki2
        else ret_kyori eki2 search_eki1


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
  let ekikan_tree = inserts_ekikan RedBlack.empty ekikan_lst
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



let test13 = time (fun () -> dijkstra "bibi" "osatsu" global_ekimei_list global_ekikan_list)
let test14 = time (fun () -> dijkstra "bibi" "naeho" global_ekimei_list global_ekikan_list )        
let test15 = time (fun () -> dijkstra "bibi" "kamaya" global_ekimei_list global_ekikan_list)
