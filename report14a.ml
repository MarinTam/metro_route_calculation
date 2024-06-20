(* r1 実行するたびに変数 shingou の値を 「次」の色に変更する unit -> unit 型の関数 *)
(* next : unit -> unit *)
type shingou_t = Red | Yellow | Green

let shingou = ref Red

let next () = match !shingou with
  | Red -> shingou := Green
  | Yellow -> shingou := Red
  | Green -> shingou := Yellow

(* 問１ 文字列を受け取ったら、その文字列に呼ばれるごとに異なる数字をつけた文字列を返す *)
(* gensym : string -> string *)
let counter = ref 0

let gensym c = counter := !counter + 1;
  c^(string_of_int (!counter-1))


let test1 = gensym "j" = "j0"
let test2 = gensym "d" = "d1"
let test3 = gensym "ahsdf" = "ahsdf2"
    
