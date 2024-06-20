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

val global_ekimei_list : ekimei_t list

val global_ekikan_list : ekikan_t list
