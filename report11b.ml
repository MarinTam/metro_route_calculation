(* #use "metro.ml" *)
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

type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
}

(* 問4 駅のリストを受け取り,最短距離最小の駅と最短距離最小の駅以外からなるリストの組みを返す *)
(* saitan_wo_bunri : eki_t -> eki_t list -> eki_t * eki_t list *)


let rec saitan_wo_bunri first rest =
  let rec saitan_wo_bunri1 stn rst =
    match rst with
    [] -> stn
    | first1 :: rest1 -> match stn with
    | {namae = n; saitan_kyori = s1; temae_list = t} -> (match first1 with
          | {namae = n2; saitan_kyori = s2; temae_list = t2} -> (if s1 <= s2 then saitan_wo_bunri1 stn rest1 else saitan_wo_bunri1 first1 rest1))
        in let saitan_wo_bunri2 eki_t =
             eki_t <> saitan_wo_bunri1 first rest
               in (saitan_wo_bunri1 first rest, List.filter saitan_wo_bunri2 (first :: rest))


let test1 = saitan_wo_bunri
    {namae = ("甲府", "山梨"); saitan_kyori = 5.5; temae_list = [("甲府", "山梨")]} [{namae = ("塩崎", "山梨"); saitan_kyori = 5.0; temae_list = [("塩崎", "山梨")]};
     {namae = ("竜王", "山梨"); saitan_kyori = 6.0; temae_list = [("竜王", "山梨")]}]
 = ({namae = ("塩崎", "山梨");saitan_kyori = 5.;temae_list = [("塩崎", "山梨")]},
      [{namae = ("甲府", "山梨");saitan_kyori = 5.5;temae_list = [("甲府", "山梨")]};
      {namae = ("竜王", "山梨");saitan_kyori = 6.;temae_list = [("竜王", "山梨")]}])
let test2 = saitan_wo_bunri 
    {namae = ("甲府", "山梨"); saitan_kyori = 4.0; temae_list = [("甲府", "山梨")]}
     [{namae = ("塩崎", "山梨"); saitan_kyori = 5.0; temae_list = [("塩崎", "山梨")]};
     {namae = ("竜王", "山梨"); saitan_kyori = 6.0; temae_list = [("竜王", "山梨")]}]
 = ({namae = ("甲府", "山梨"); saitan_kyori = 4.;temae_list = [("甲府", "山梨")]},
   [{namae = ("塩崎", "山梨"); saitan_kyori = 5.;temae_list = [("塩崎", "山梨")]};
    {namae = ("竜王", "山梨"); saitan_kyori = 6.;temae_list = [("竜王", "山梨")]}])
let test3 = saitan_wo_bunri 
    {namae = ("甲府", "山梨"); saitan_kyori = 5.5; temae_list = [("甲府", "山梨")]}
     [{namae = ("塩崎", "山梨"); saitan_kyori = 9.6; temae_list = [("塩崎", "山梨")]};
     {namae = ("竜王", "山梨"); saitan_kyori = 6.0; temae_list = [("竜王", "山梨")]}]
 = ({namae = ("甲府", "山梨"); saitan_kyori = 5.5;temae_list = [("甲府", "山梨")]},
   [{namae = ("塩崎", "山梨"); saitan_kyori = 9.6;temae_list = [("塩崎", "山梨")]};
    {namae = ("竜王", "山梨"); saitan_kyori = 6.;temae_list = [("竜王", "山梨")]}])
(* let test4 = saitan_wo_bunri2 eki_t_list_ex *)
