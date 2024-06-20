
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




