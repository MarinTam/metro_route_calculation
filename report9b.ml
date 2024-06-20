#use "global.ml"
  (* #use "global.ml" *)
  (* #use "metro.ml" *)
(* #use "kansai.ml" *)
(* type ekimei_t = {
  kanji   : string; (* 漢字の駅名 *)
  kana    : string; (* 読み *)
  romaji  : string; (* ローマ字 *)
  ken     : string; (* 県名 *)
  shozoku : string; (* 所属路線名 *)
  } *)

(* 駅間の情報を格納するレコード型 *)
(* type ekikan_t = {
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
let make_initial_eki_list2 ekimei_lst ekimei_pair =
  let make_initial_eki_list1 ekimei =
    match ekimei with
    | {kanji = kanji2_v; kana = kana2_v; romaji = romaji2_v; ken = ken2_v; shozoku = shozoku2_v}
      -> (if (kanji2_v, ken2_v)
             = ekimei_pair then {namae = (kanji2_v, ken2_v); saitan_kyori = 0.; temae_list = [ekimei_pair]}
          else {namae = (kanji2_v, ken2_v); saitan_kyori = infinity; temae_list = []})
  in List.map make_initial_eki_list1 ekimei_lst
(* n回 *)

let rec get_ekikan_kyori2 eki1 eki2 ekikan_lst =
  match ((eki1, eki2), ekikan_lst) with
  | (pair, []) -> infinity
  | (((ekimei1, ken1), (ekimei2, ken2)), ({kiten = kiten2_v; kenk = kenk2_v; shuten = shuten2_v; kens = kens2_v; keiyu = keiyu2_v; kyori = kyori2_v; jikan = jikan2_v} :: rest)) -> (if ekimei1 = kiten2_v && ken1 = kenk2_v && ekimei2 = shuten2_v && ken2 = kens2_v || ekimei1 = shuten2_v && ken1 = kens2_v && ekimei2 = kiten2_v && ken2 = kenk2_v then kyori2_v else get_ekikan_kyori2 eki1 eki2 rest)
(* n/2 *)

let rec koushin p v lst =
  let koushin1 p q lst =
    match p with
    | {namae = namae4_v; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v} -> (match q with
        | {namae = namae_v; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v} -> (let kyori = get_ekikan_kyori2 namae4_v namae_v lst +. saitan_kyori2_v in (if kyori = infinity then q else if saitan_kyori_v >= kyori then {namae = namae_v; saitan_kyori = kyori; temae_list = namae_v :: temae_list2_v} else q)))
  in (match v with
      | [] -> []
      | first :: rest -> koushin1 p first lst :: koushin p rest lst)
(* n^2 *)

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
(* n^2 *)
let rec dijkstra_main eki_t_lst ekikan_t_lst =
  match eki_t_lst with
  | [] -> []
  | first :: rest -> (match saitan_wo_bunri2 eki_t_lst with
      | (p, v) -> (match v with
          | [] -> [p]
          | first1 :: rest2 -> p :: dijkstra_main (koushin p v ekikan_t_lst) ekikan_t_lst))
(* n^3 *)

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
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest4 -> (if romaji3_v = romaji then (kanji3_v, ken3_v)
                                                                                                            else romaji_to_kanji2 romaji rest4)
                                                                                                           
let dijkstra kiten_ekimei shuten_ekimei ekimei_lst ekikan_lst =
  let kiten_pair = romaji_to_kanji2 kiten_ekimei ekimei_lst
  in (let shuten_pair = romaji_to_kanji2 shuten_ekimei ekimei_lst
  in (let seiretsugo = seiretsu2 ekimei_lst
  in (let init_eki_t_lst = make_initial_eki_list2 seiretsugo kiten_pair
  in (let saitan_lst = dijkstra_main init_eki_t_lst ekikan_lst
  in (let rec pick_eki_t pair lst =
  match lst with
  | [] -> {namae = ("foo", "foo"); saitan_kyori = infinity; temae_list = []}
  | first :: rest -> (match first with
  | {namae = namae2_v; saitan_kyori = saitan_kyori3_v; temae_list = temae_list3_v} -> (if namae2_v = pair then first
  else pick_eki_t pair rest))
      in pick_eki_t shuten_pair saitan_lst)))))

(* time : (unit -> 'a) -> 'a * float *)
let time f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time  = Sys.time () in
  (result, end_time -. start_time)

(*
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
            *)


let test7 = time (fun () -> dijkstra "kofu" "okaya" global_ekimei_list global_ekikan_list)
let test8 = time (fun () -> dijkstra "kofu" "anayama" global_ekimei_list global_ekikan_list )
let test9 = time (fun () -> dijkstra "kohu" "hinoharu" global_ekimei_list global_ekikan_list)
            

(*
let test10 = time (fun () -> dijkstra "omiya" "katsura" global_ekimei_list global_ekikan_list)
let test11 = time (fun () -> dijkstra "omiya" "kamikatsura" global_ekimei_list global_ekikan_list )
let test12 = time (fun () -> dijkstra "omiya" "sojiji" global_ekimei_list global_ekikan_list)
             *)
(*
let test13 = time (fun () -> dijkstra "bibi" "osatsu" global_ekimei_list global_ekikan_list)
let test14 = time (fun () -> dijkstra "bibi" "naeho" global_ekimei_list global_ekikan_list )
let test15 = time (fun () -> dijkstra "bibi" "kamaya" global_ekimei_list global_ekikan_list)







let sum a b = a + b
let sum_line c d = c^d
let mul e f = e*f
(* 問５　関数fと初期値およびリストを受け取ったらinitを初期値としlstの各要素に左からfをかけていく *)
(* 例えば fold_left f init [a1; a2; a3] ならf (f (f init a1) a2) a3を計算 *)
(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a  *)
let rec fold_left f init lst =
  let rec pick_last lst =
    match lst with
  | [] -> pick_last []
  | first :: rest -> (if List.length rest = 0 then first
                      else pick_last rest)
  in (let rec pick_rest lst =
  match lst with
  | [] -> []
  | first :: rest -> (if List.length rest = 0 then pick_rest rest
                      else first :: pick_rest rest)
  in (match lst with
    [] -> init
    | first :: rest ->
      f (fold_left f init (pick_rest lst)) (pick_last lst)))


let test1 = fold_left sum 0 [3;4;5] = 12
let test2 = fold_left sum_line "" ["I";"love";"USA"] = "IloveUSA"
let test3 = fold_left mul 1 [7;3;8;8;3;3;4;5;4;6] = 5806080
*)
