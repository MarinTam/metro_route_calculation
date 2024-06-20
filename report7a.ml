 (* #use "global.ml";; *)

 type ekikan_t = {
  kiten : string;	(* 起点 *)
  kenk : string;	(* 起点の県名 *)
  shuten : string;	(* 終点 *)
  kens : string;	(* 終点の県名 *)
  keiyu : string;	(* 経由路線名 *)
  kyori : float;	(* 距離 *)
  jikan : int;	(* 所要時間 *)
} 

type eki_t = {
  namae : string * string;
  saitan_kyori : float;
  temae_list : (string * string) list;
}

let rec get_ekikan_kyori2 eki1 eki2 ekikan_lst =
  match ((eki1, eki2), ekikan_lst) with
  | (pair, []) -> infinity
  | (((ekimei1, ken1), (ekimei2, ken2)), ({kiten = kiten2_v; kenk = kenk2_v; shuten = shuten2_v; kens = kens2_v; keiyu = keiyu2_v; kyori = kyori2_v; jikan = jikan2_v} :: rest)) -> (if ekimei1 = kiten2_v && ken1 = kenk2_v && ekimei2 = shuten2_v && ken2 = kens2_v || ekimei1 = shuten2_v && ken1 = kens2_v && ekimei2 = kiten2_v && ken2 = kenk2_v then kyori2_v
  else get_ekikan_kyori2 eki1 eki2 rest)

(* let rec make_eki_list2 lst =
  match lst with
  | [] -> []
  | {kanji = kanji3_v; kana = kana3_v; romaji = romaji3_v; ken = ken3_v; shozoku = shozoku3_v} :: rest3 -> {namae = (kanji3_v, ken3_v); saitan_kyori = infinity; temae_list = []} :: make_eki_list2 rest3
 *)

(* let global_eki_list = make_eki_list2 global_ekimei_list *)

(* 問１ ダイクストラ法で最短距離を更新する。点p,qと駅間リストを受け取りqpの接続を調べ,接続していたら新しいqを返す.
qが保持する最短距離と,p経由でqに行った距離のうち短い方を使う.
p経由の方が短かった場合,pの手前リストの先頭にqを加えたものをqのtemae_listにする *)
(* koushin1 : eki_t -> eki_t -> ekikan_t list -> eki_t list *)
let koushin1 p q lst =
  match p with
  | {namae = namae4_v; saitan_kyori = saitan_kyori2_v; temae_list = temae_list2_v} -> (match q with
  | {namae = namae_v; saitan_kyori = saitan_kyori_v; temae_list = temae_list_v} -> (let kyori = get_ekikan_kyori2 namae4_v namae_v lst +. saitan_kyori2_v
  in (if kyori = infinity then q
  else if saitan_kyori_v > kyori then {namae = namae_v; saitan_kyori = kyori; temae_list = namae_v :: temae_list2_v}
  else q)))

(* テスト *)
let test1 = koushin1 {namae = ("甲府", "山梨"); saitan_kyori = 1.0; temae_list = []}
                     {namae = ("竜王", "山梨"); saitan_kyori = infinity; temae_list = []}
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.1; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4}]
 = ({namae = ("竜王", "山梨"); saitan_kyori = 5.5; temae_list = [("竜王", "山梨")]})
let test2 = koushin1 ({namae = ("竜王", "山梨"); saitan_kyori = 2.0; temae_list = []})
            ({namae = ("塩崎", "山梨"); saitan_kyori = 6.0; temae_list = []})
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.1; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4}]
 = ({ namae = ("塩崎", "山梨"); saitan_kyori = 6.0; temae_list = [] })
let test3 = koushin1 ({namae = ("甲府", "山梨"); saitan_kyori = 0.0; temae_list = []})
            ({namae = ("塩崎", "山梨"); saitan_kyori = 7.0; temae_list = []})
[{kiten="甲府"; kenk="山梨"; shuten="竜王"; kens="山梨"; keiyu="中央本線"; kyori=4.5; jikan=4};
{kiten="竜王"; kenk="山梨"; shuten="塩崎"; kens="山梨"; keiyu="中央本線"; kyori=4.1; jikan=3};
{kiten="塩崎"; kenk="山梨"; shuten="韮崎"; kens="山梨"; keiyu="中央本線"; kyori=4.3; jikan=4};
{kiten="韮崎"; kenk="山梨"; shuten="新府"; kens="山梨"; keiyu="中央本線"; kyori=4.2; jikan=4};]
 = ({ namae = ("塩崎", "山梨"); saitan_kyori = 7.0; temae_list = [] })
(* let test4 = ({namae = ("甲府", "山梨"); saitan_kyori = infinity; temae_list = []}) 
            ({namae = ("竜王", "山梨"); saitan_kyori = infinity; temae_list = []}) global_ekikan_list *)


(* 問２ 点pと最短距離が未確定の点の集合V,および駅間のリストを受け取りV中の全ての駅について必要に応じて更新処理を行った後の未確定の駅の集合を返す *)
(* koushin : eki_t -> eki_t list -> ekikan_t list -> eki_t list *)
let rec koushin p v lst =
  match v with
  | [] -> []
  | first :: rest -> koushin1 p first lst :: koushin p rest lst

(* テスト *)
(* let test4 = koushin {namae = ("甲府", "山梨"); saitan_kyori = 1.2; temae_list = []} 
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
