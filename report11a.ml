#use "metro.ml";;
exception No_such_station of string 

(* type ekimei_t = {
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


(* 問１　get_ekikan_kyori2を２駅がつながっていない時,例外Not_foundを起こすよう変更 *)
(* string * string -> string * string -> ekikan_t list -> float *)
let get_ekikan_kyori2 eki1 eki2 ekikan_lst =
  let rec get_ekikan_kyori eki1 eki2 ekikan_lst =
  match ((eki1, eki2), ekikan_lst) with
  | (pair, []) -> None
  | (((ekimei1, ken1), (ekimei2, ken2)), ({kiten = kiten2_v; kenk = kenk2_v; shuten = shuten2_v; kens = kens2_v; keiyu = keiyu2_v; kyori = kyori2_v; jikan = jikan2_v} :: rest)) -> (if ekimei1 = kiten2_v && ken1 = kenk2_v && ekimei2 = shuten2_v && ken2 = kens2_v || ekimei1 = shuten2_v && ken1 = kens2_v && ekimei2 = kiten2_v && ken2 = kenk2_v then Some(kyori2_v) else get_ekikan_kyori eki1 eki2 rest)
in (match get_ekikan_kyori eki1 eki2 ekikan_lst with
    None -> raise Not_found
     | Some(kyori) -> kyori)
   
(*
let test1 = get_ekikan_kyori2 ("上野","東京") ("上野広小路","東京") global_ekikan_list = 0.5
let test2 = get_ekikan_kyori2 ("上野広小路","東京") ("上野","東京") global_ekikan_list = 0.5
 let test3 = get_ekikan_kyori2 ("田原","東京") ("上野","東京") global_ekikan_list *)

(* 問2 問1で作ったget_ekikan_kyori2を使ってkoushin1とkoushinを書き換える.通常の処理として,着目している駅経由の方が近かったらそちらに更新,例外的に繋がっていなかったら何もせず返す *)
(* eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let rec koushin p v lst =
  let koushin1 p q lst =
    match p with
    | {namae = namae4_v; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v} -> match q with
      | {namae = namae_v; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v} ->
 (try
  (let kyori = get_ekikan_kyori2 namae4_v namae_v lst +. saitan_kyori2_v
   in (if saitan_kyori_v >= kyori then {namae = namae_v; saitan_kyori = kyori;
             temae_list = namae_v :: temae_list2_v} else q))
  with Not_found -> q)
  in (match v with
      | [] -> []
      | first :: rest -> koushin1 p first lst :: koushin p rest lst)


(* let test4 = koushin {namae = ("明治神宮前", "東京"); saitan_kyori = 1.2; temae_list = []} 
 global_eki_list global_ekikan_list *)

let test1 = koushin ({namae = ("甲府", "山梨"); saitan_kyori = 1.0; temae_list = []}) 
 [{namae = ("竜王", "山梨"); saitan_kyori = infinity; temae_list = []};
  {namae = ("塩崎", "山梨"); saitan_kyori = infinity; temae_list = []};] 
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.1; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};]
  = ([{namae = ("竜王", "山梨"); saitan_kyori = 5.5;temae_list = [("竜王", "山梨")]};
    {namae = ("塩崎", "山梨"); saitan_kyori = infinity; temae_list = []}])
let test2 = koushin({namae = ("竜王", "山梨"); saitan_kyori = 1.0; temae_list = []}) 
 [{namae = ("甲府", "山梨"); saitan_kyori = infinity; temae_list = []};
  {namae = ("塩崎", "山梨"); saitan_kyori = infinity; temae_list = []};] 
  [{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.0; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};]
  = ([{namae = ("甲府", "山梨"); saitan_kyori = 5.5; temae_list = [("甲府", "山梨")]}; 
     {namae = ("塩崎", "山梨"); saitan_kyori = 5.0; temae_list = [("塩崎", "山梨")]}])
let test3 = koushin ({namae = ("塩崎", "山梨"); saitan_kyori = 2.0; temae_list = []}) 
 [{namae = ("甲府", "山梨"); saitan_kyori = infinity; temae_list = []};
  {namae = ("竜王", "山梨"); saitan_kyori = infinity; temae_list = []};] 
  [{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.0; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};]
 = ([{ namae = ("甲府", "山梨"); saitan_kyori = infinity; temae_list = []};
    {namae = ("竜王", "山梨"); saitan_kyori = 6.0; temae_list = [("竜王", "山梨")]}])

                 

(* 問３　ローマ字で入力した駅を駅名ペアで返す *)
(* 見つからなかったら例外 No_such_station を起こす *)
(* romaji_to_kanji2 : string -> ekimei_t list -> string*string *)
let rec romaji_to_kanji2 romaji lst =
  match lst with
  | [] -> raise (No_such_station (romaji))
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest4 -> (if romaji3_v = romaji then (kanji3_v, ken3_v)
  else romaji_to_kanji2 romaji rest4)

(*
let romaji_to_kanji2 romaji lst =
  try romaji_to_kanji romaji lst 
  with No_such_station (romaji) -> (romaji,"")
*)
 
let test1 = romaji_to_kanji2 "meijijingumae" global_ekimei_list = (("明治神宮前", "東京"))
let test2 = romaji_to_kanji2 "yoyogiuehara" global_ekimei_list = (("代々木上原", "東京"))
let test3 = romaji_to_kanji2 "zoshigaya" global_ekimei_list = (("雑司が谷", "東京"))
let test4 = romaji_to_kanji2 "AAA" global_ekimei_list


