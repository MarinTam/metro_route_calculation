#use "metro.ml" ;;

(* type ekimei_t = {
  kanji : string;	(* 漢字の駅名 *)
  kana : string;	(* 読み *)
  romaji : string;	(* ローマ字 *)
  ken : string;	(* 県名 *)
  shozoku : string;	(* 所属路線名 *)
}

type ekikan_t = {
  kiten : string;	(* 起点 *)
  kenk : string;	(* 起点の県名 *)
  shuten : string;	(* 終点 *)
  kens : string;	(* 終点の県名 *)
  keiyu : string;	(* 経由路線名 *)
  kyori : float;	(* 距離 *)
  jikan : int;	(* 所要時間 *)
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

let rec get_ekikan_kyori2 eki1 eki2 ekikan_lst =
  match ((eki1, eki2), ekikan_lst) with
  | (pair, []) -> infinity
  | (((ekimei1, ken1), (ekimei2, ken2)), ({kiten = kiten2_v; kenk = kenk2_v; shuten = shuten2_v; kens = kens2_v; keiyu = keiyu2_v; kyori = kyori2_v; jikan = jikan2_v} :: rest)) -> (if ekimei1 = kiten2_v && ken1 = kenk2_v && ekimei2 = shuten2_v && ken2 = kens2_v || ekimei1 = shuten2_v && ken1 = kens2_v && ekimei2 = kiten2_v && ken2 = kenk2_v then kyori2_v else get_ekikan_kyori2 eki1 eki2 rest)

let rec koushin p v lst =
  let koushin1 p q lst =
    match p with
    | {namae = namae4_v; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v} -> (match q with
        | {namae = namae_v; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v} -> (let kyori = get_ekikan_kyori2 namae4_v namae_v lst +. saitan_kyori2_v
                                                                                          in (if kyori = infinity then q
                                                                                              else if saitan_kyori_v > kyori then {namae = namae_v; saitan_kyori = kyori; temae_list = namae_v :: temae_list2_v}
                                                                                              else q)))
  in (match v with
      | [] -> []
      | first :: rest -> koushin1 p first lst :: koushin p rest lst)


let saitan_wo_bunri2 eki_t_lst =
  let pick_saitan2 eki_t1 eki_t2 =
    match eki_t2 with
    | {namae = namae8_v; saitan_kyori = saitan_kyori8_v; temae_list = temae_list8_v} -> (match eki_t1 with
        | {namae = namae7_v; saitan_kyori = saitan_kyori7_v; temae_list = temae_list7_v} -> (if saitan_kyori7_v < saitan_kyori8_v then eki_t1
                                                                                             else eki_t2))
  in (let pick_saitan1 eki_t_lst2 =
        List.fold_right pick_saitan2 eki_t_lst2 {namae = ("", ""); saitan_kyori = infinity; temae_list = []}
      in (let saitan_wo_bunri1 eki_t =
            eki_t <> pick_saitan1 eki_t_lst
          in (pick_saitan1 eki_t_lst, List.filter saitan_wo_bunri1 eki_t_lst)))

(* 問1 受け取った整数のリストを昇順に並べる *)
(* quick_sort : int list -> int list *)
(* let rec quick_sort lst =
  let rec take_less n lst =
    match lst with
      [] -> []
    | first :: rest ->
      (if first < n then first :: take_less n rest else take_less n rest)
  in (let rec take_greater n lst =
        match lst with
          [] -> []
        | first :: rest ->
     (if first > n then first :: take_greater n rest else take_greater n rest)
  in (match lst with
    [] -> []
  | first :: rest ->
    quick_sort ( take_less first rest)
    @ [first]
    @ quick_sort (take_greater first rest)))

let test1 = quick_sort [4;7;3;2;6;2;2]
let test2 = quick_sort []
let test3 = quick_sort [4;4;4;4;4;4;4;6;0;0;4]

            *)



(* 問2 ダイクストラ法を使い、起点のみ最短距離が0で他はinfinityの駅のリストと駅間リストを受け取ったら「起点からの最短距離と『起点からその駅に至る駅名のリスト』が入った駅」のリストを返す *)
(* dijkstra_main : eki_t list -> ekikan_t list -> eki_t list *)

let rec dijkstra_main eki_t_lst ekikan_t_lst =
  match eki_t_lst with
  | [] -> []
  | first :: rest -> (match saitan_wo_bunri2 eki_t_lst with
      | (p, v) -> (match v with
          | [] -> [p]
          | first1 :: rest2 -> p :: dijkstra_main (koushin p v ekikan_t_lst) ekikan_t_lst))

let test1 = dijkstra_main 
    [{namae = ("甲府", "山梨"); saitan_kyori = infinity; temae_list = []};
     {namae = ("竜王", "山梨"); saitan_kyori = infinity;temae_list = []};
     {namae = ("塩崎", "山梨"); saitan_kyori = 0.;temae_list = [("塩崎", "山梨")]};
     {namae = ("韮崎", "山梨"); saitan_kyori = infinity; temae_list = []}]
    [{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨";
      keiyu="中央本線"; kyori=4.5; jikan=4};
     {kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨";
      keiyu="中央本線"; kyori=4.1; jikan=3};
     {kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨";
      keiyu="中央本線"; kyori=4.3; jikan=4};
     {kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨";
      keiyu="中央本線"; kyori=4.2; jikan=4};]
            = [{namae = ("塩崎", "山梨"); saitan_kyori = 0.;
                temae_list = [("塩崎", "山梨")]};
               {namae = ("竜王", "山梨"); saitan_kyori = 4.1;
                temae_list = [("竜王", "山梨"); ("塩崎", "山梨")]};
               {namae = ("韮崎", "山梨"); saitan_kyori = 4.3;
                temae_list = [("韮崎", "山梨"); ("塩崎", "山梨")]};
               {namae = ("甲府", "山梨"); saitan_kyori = 8.6;
                temae_list = [("甲府", "山梨"); ("竜王", "山梨"); ("塩崎","山梨")]}]
let test2 = dijkstra_main 
    [{namae = ("甲府", "山梨"); saitan_kyori = 0.; temae_list = [("甲府","山梨")]};
     {namae = ("竜王", "山梨"); saitan_kyori = infinity;temae_list = []};
     {namae = ("塩崎", "山梨"); saitan_kyori = infinity; temae_list = []};
     {namae = ("韮崎", "山梨"); saitan_kyori = infinity; temae_list = []}]
    [{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨";
      keiyu="中央本線"; kyori=4.5; jikan=4};
     {kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨";
      keiyu="中央本線"; kyori=4.0; jikan=3};
     {kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨";
      keiyu="中央本線"; kyori=4.3; jikan=4};
     {kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨";
      keiyu="中央本線"; kyori=4.2; jikan=4};]
    = [{namae = ("甲府", "山梨"); saitan_kyori = 0.;
    temae_list = [("甲府", "山梨")]};
   {namae = ("竜王", "山梨"); saitan_kyori = 4.5;
    temae_list = [("竜王", "山梨"); ("甲府", "山梨")]};
   {namae = ("塩崎", "山梨"); saitan_kyori = 8.5;
    temae_list =
     [("塩崎", "山梨"); ("竜王", "山梨"); ("甲府", "山梨")]};
   {namae = ("韮崎", "山梨"); saitan_kyori = 12.8;
    temae_list =
     [("韮崎", "山梨"); ("塩崎", "山梨"); ("竜王", "山梨");
      ("甲府", "山梨")]}]           
let test3 = dijkstra_main 
    [{namae = ("甲府", "山梨"); saitan_kyori = 0.; temae_list = [("塩崎","山梨")]};
     {namae = ("竜王", "山梨"); saitan_kyori = infinity;temae_list = []};
     {namae = ("塩崎", "山梨"); saitan_kyori = infinity; temae_list = []};
     {namae = ("韮崎", "山梨"); saitan_kyori = infinity; temae_list = []}]
    [{kiten="韮崎"; kenk="山梨"; shuten="竜王"; kens="山梨";
      keiyu="中央本線"; kyori=1.0; jikan=4};
     {kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨";
      keiyu="中央本線"; kyori=2.4; jikan=3};
     {kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨";
      keiyu="中央本線"; kyori=1.2; jikan=4};
     {kiten="甲府"; kenk="山梨"; shuten="塩崎"; kens="山梨";
      keiyu="中央本線"; kyori=4.2; jikan=4};]
    = [{namae = ("甲府", "山梨"); saitan_kyori = 0.;
    temae_list = [("塩崎", "山梨")]};
   {namae = ("塩崎", "山梨"); saitan_kyori = 4.2;
    temae_list = [("塩崎", "山梨"); ("塩崎", "山梨")]};
   {namae = ("韮崎", "山梨"); saitan_kyori = 5.4;
    temae_list =
     [("韮崎", "山梨"); ("塩崎", "山梨"); ("塩崎", "山梨")]};
   {namae = ("竜王", "山梨"); saitan_kyori = 6.4;
    temae_list =
     [("竜王", "山梨"); ("韮崎", "山梨"); ("塩崎", "山梨");
      ("塩崎", "山梨")]}]
let test4 = dijkstra_main [] [] = []
      
(* let test_eki_lst = make_initial_eki_list2 global_ekimei_list ("韮崎", "山梨") 
   let test_global = dijkstra_main test_eki_lst global_ekikan_list *)

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
  | first :: rest -> insert (sort rest) first
      in sort lst))

let rec romaji_to_kanji2 romaji lst =
  match lst with
  | [] -> ("", "")
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest4 -> (if romaji3_v = romaji then (kanji3_v, ken3_v)
  else romaji_to_kanji2 romaji rest4)

(* 問３ 起点と終点の駅名と駅名リスト,駅間リストを受け取りromaji_to_kanji2で起点と終点の駅名ペアを求め、seiretsu2で駅名リストの重複を取り除き、make_initial_eki_list2で駅のリスト（eki_t )を作り、dijkstra_mainで各駅までの最短路を確定、その中から終点を探して返す。起点と終点の駅名と駅名リスト、駅間リストを受け取ったら最短ルートと距離を返す *)
 (* dijkstra : string -> string -> ekimei_t list -> ekikan_t list -> eki_t *)

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

let test1 = dijkstra "kofu" "anayama"
    [{kanji="甲府"; kana="こうふ"; romaji="kofu"; ken="山梨"; shozoku="中央本線"};
{kanji="竜王"; kana="りゅうおう"; romaji="ryuou"; ken="山梨"; shozoku="中央本線"};
{kanji="塩崎"; kana="しおざき"; romaji="shiozaki"; ken="山梨"; shozoku="中央本線"};
{kanji="韮崎"; kana="にらさき"; romaji="nirasaki"; ken="山梨"; shozoku="中央本線"};
{kanji="新府"; kana="しんぷ"; romaji="shinpu"; ken="山梨"; shozoku="中央本線"};
{kanji="穴山"; kana="あなやま"; romaji="anayama"; ken="山梨"; shozoku="中央本線"}]
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.0; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};
 {kiten="新府"; kenk="山梨"; shuten="穴山"; kens="山梨"; keiyu="中央本線"; kyori=3.5; jikan=3}]
= {namae = ("穴山", "山梨"); saitan_kyori = 20.5;
   temae_list =
    [("穴山", "山梨"); ("新府", "山梨"); ("韮崎", "山梨");
     ("塩崎", "山梨"); ("竜王", "山梨"); ("甲府", "山梨")]}
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
let test5 = dijkstra "nirasaki" "ryuou"
    [{kanji="甲府"; kana="こうふ"; romaji="kofu"; ken="山梨"; shozoku="中央本線"};
{kanji="竜王"; kana="りゅうおう"; romaji="ryuou"; ken="山梨"; shozoku="中央本線"};
{kanji="塩崎"; kana="しおざき"; romaji="shiozaki"; ken="山梨"; shozoku="中央本線"};
{kanji="韮崎"; kana="にらさき"; romaji="nirasaki"; ken="山梨"; shozoku="中央本線"};
{kanji="新府"; kana="しんぷ"; romaji="shinpu"; ken="山梨"; shozoku="中央本線"};
{kanji="穴山"; kana="あなやま"; romaji="anayama"; ken="山梨"; shozoku="中央本線"}]
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.0; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};
 {kiten="新府"; kenk="山梨"; shuten="穴山"; kens="山梨"; keiyu="中央本線"; kyori=3.5; jikan=3}]
 = {namae = ("竜王", "山梨"); saitan_kyori = 8.3;
   temae_list =
    [("竜王", "山梨"); ("塩崎", "山梨"); ("韮崎", "山梨")]}
let test6 = dijkstra "shinpu" "kofu"
 [{kanji="甲府"; kana="こうふ"; romaji="kofu"; ken="山梨"; shozoku="中央本線"};
{kanji="竜王"; kana="りゅうおう"; romaji="ryuou"; ken="山梨"; shozoku="中央本線"};
{kanji="塩崎"; kana="しおざき"; romaji="shiozaki"; ken="山梨"; shozoku="中央本線"};
{kanji="韮崎"; kana="にらさき"; romaji="nirasaki"; ken="山梨"; shozoku="中央本線"};
{kanji="新府"; kana="しんぷ"; romaji="shinpu"; ken="山梨"; shozoku="中央本線"};
{kanji="穴山"; kana="あなやま"; romaji="anayama"; ken="山梨"; shozoku="中央本線"}]
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.0; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};
 {kiten="新府"; kenk="山梨"; shuten="穴山"; kens="山梨"; keiyu="中央本線"; kyori=3.5; jikan=3}]
 = {namae = ("甲府", "山梨"); saitan_kyori = 17.;
   temae_list =
    [("甲府", "山梨"); ("竜王", "山梨"); ("塩崎", "山梨");
     ("韮崎", "山梨"); ("新府", "山梨")]}

(* let test4 = dijkstra "nishi-funabashi" "gokokuji" global_ekimei_list global_ekikan_list *)
