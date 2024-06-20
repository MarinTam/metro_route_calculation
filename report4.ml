#use "metro.ml";;

(* 問１・・・リストを受け取ったらそのリストの長さを返す *)
(* length : 'a list -> int *)
let rec length lst = match lst with
    [] -> 0
    | first :: rest -> 1 + length rest

(* テスト *)
let test1 = length [5; 4; 6] = 3
let test2 = length [] = 0
let test3 = length ["A"; "B"; "C" ; "D"] = 4
let test4 = length global_ekimei_list


(* 問２・・・ローマ字の駅名と駅名リスト（ekimei_t型）を受け取ったら、その駅の漢字表記と県名のペアを返す *)
(* romaji_to_kanji : string -> ekimei_t list -> string*string *)
let rec romaji_to_kanji2 romaji lst =
  match lst with
  | [] -> ("", "")
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest4 -> (if romaji3_v = romaji then (kanji3_v, ken3_v)
  else romaji_to_kanji2 romaji rest4)

(* テスト *)
let test1 = romaji_to_kanji2 "meijijingumae" global_ekimei_list = (("明治神宮前", "東京"))
let test2 = romaji_to_kanji2 "yoyogiuehara" global_ekimei_list = (("代々木上原", "東京"))
let test3 = romaji_to_kanji2 "zoshigaya" global_ekimei_list = (("雑司が谷", "東京"))
let test4 = romaji_to_kanji2 "AAA" global_ekimei_list = (("", ""))


(* 問３・・・駅名ペアを２つと駅間リストを受け取って来たら、駅間リストの中からその２駅（ペア）間の距離を返す *)
(* get_ekikan_kyori2 : string*string -> string*string -> ekikan_t list -> float *)
let rec get_ekikan_kyori2 eki1 eki2 ekikan_lst =
  match ((eki1,eki2), ekikan_lst) with
  | (pair, []) -> infinity
  | (((ekimei1, ken1), (ekimei2, ken2)), ({kiten = kiten2_v; kenk = kenk2_v; shuten = shuten2_v; kens = kens2_v; keiyu = keiyu2_v; kyori = kyori2_v; jikan = jikan2_v} :: rest)) -> (if ekimei1 = kiten2_v && ken1 = kenk2_v && ekimei2 = shuten2_v && ken2 = kens2_v || ekimei1 = shuten2_v && ken1 = kens2_v && ekimei2 = kiten2_v && ken2 = kenk2_v then kyori2_v
  else get_ekikan_kyori2 eki1 eki2 rest)

(* テスト *)
let test1 = get_ekikan_kyori2 ("上野","東京") ("上野広小路","東京") global_ekikan_list = 0.5
let test2 = get_ekikan_kyori2 ("上野広小路","東京") ("上野","東京") global_ekikan_list = 0.5
let test3 = get_ekikan_kyori2 ("田原","東京") ("上野","東京") global_ekikan_list = infinity


(* r1 *)
let rec contain lst n =
  match lst with
  | [] -> false
  | first2 :: rest2 -> (if first2 = n then true
  else contain rest2 n)
let test1 = contain [] 0 = false
let test2 = contain [0; 0; 0] 5 = false
let test3 = contain [0; 0; 5] 5 = true

(* r2 *)
let rec count lst n =
  match lst with
  | [] -> 0
  | first5 :: rest6 -> (if n = first5 then 1 + count rest6 n
  else count rest6 n)
let test1 = count [1; 1; 0] 1 = 2
let test2 = count [0; 0; 0] 1 = 0
let test3 = count [1; 1; 1] 1 = 3
