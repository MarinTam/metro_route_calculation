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
