(* r2 *)
(* type tree_t = Empty | Node of tree_t * char * int * tree_t

let rec search tree c =
  match tree with
  | Empty -> -1
  | Node ( l, key, v, r) -> if key = c then v else if search r c = -1 then search l c else search r c
*)


type ('a, 'b) tree_t = Empty
                     | Node of ('a, 'b) tree_t * 'a * 'b * ('a, 'b) tree_t

(* 問１　('a, 'b) tree_t 型の木と 'a 型のキー、 'b 型の値を受け取ったら、 そのキーと値を挿入した木を返す *)
(* insert : ('a, 'b) tree_t -> 'a -> 'b -> ('a, 'b) tree_t *)
let rec insert tree key v =
  match tree with
  | Empty -> Node(Empty,key,v,Empty)
  | Node(l,a,b,r) ->if a = key then Node(l,key,v,r) else if a<key then Node(l,a,b,insert r key v) else Node(insert l key v,a,b,r)


let t1 = Node(Empty, "g",7,Empty)
let t2 = Node (Node (Empty, "f", 6, Empty), "g", 7, Empty)
let t3 = Node (Node (Empty, "f", 6, Empty), "g", 7, Node (Empty, "j", 10, Empty))
    
let test1 = insert t1 "f" 6 = Node (Node (Empty, "f", 6, Empty), "g", 7, Empty)
let test2 = insert t2 "j" 10
     = Node (Node (Empty, "f", 6, Empty), "g", 7, Node (Empty, "j", 10, Empty))
let test3 = insert t3 "k" 11
    = Node (Node (Empty, "f", 6, Empty), "g", 7,
   Node (Empty, "j", 10, Node (Empty, "k", 11, Empty)))

(* 問2 キーと値をひとつではなく「キーと値のペア」をリストで受け取り、それを順に挿入した木を返す *)
(*  inserts : ('a, 'b) tree_t -> ('a * 'b) list -> ('a, 'b) tree_t  *)
let rec inserts tree lst =
  let rec insert1 t1 k1 v1 =
    match t1 with
  | Empty -> Node(Empty,k1,v1,Empty)
  | Node(l,a,b,r) ->if a = k1 then Node(l,k1,v1,r) else if a<k1 then Node(l,a,b,insert1 r k1 v1) else Node(insert1 l k1 v1,a,b,r)
  in let insert2 t2 pair = 
    match pair with
      | (key,v) -> insert1 t2 key v
  in match lst with
  | [] -> Empty
  | first :: rest -> if inserts (insert2 tree first) rest = Empty then insert2 tree first else  inserts (insert2 tree first) rest

let t4 = Node(Node(Node(Empty,"b",2,Empty),"f",6,Node(Empty,"h",8,Empty)),"k",11,Empty)
let t5 = inserts t4 [("i",9);("e",5);("r",18);("v",22)]
let t6 = inserts t4 [("q",17);("p",16);("s",19);("l",12)]
    
let test4 = inserts t4 [("g",7);("a",1);("j",10);("d",4)]
    = Node (Node (Node (Node (Empty, "a", 1, Empty), "b", 2, Node (Empty, "d", 4, Empty)),"f", 6, Node (Node (Empty, "g", 7, Empty), "h", 8, Node (Empty, "j", 10, Empty))), "k", 11, Empty)
let test5 = inserts t4 [("i",9);("e",5);("r",18);("v",22)]
    = Node (Node (Node (Empty, "b", 2, Node (Empty, "e", 5, Empty)), "f", 6,
     Node (Empty, "h", 8, Node (Empty, "i", 9, Empty))),
   "k", 11, Node (Empty, "r", 18, Node (Empty, "v", 22, Empty)))
let test6 = inserts t4 [("q",17);("p",16);("s",19);("l",12)]
    = Node (Node (Node (Empty, "b", 2, Empty), "f", 6, Node (Empty, "h", 8, Empty)),"k", 11, Node (Node (Node (Empty, "l", 12, Empty), "p", 16, Empty), "q", 17,
    Node (Empty, "s", 19, Empty)))
    

(* 問3 ('a, 'b) tree_t 型の木と 'a 型のキーを 受け取ったら、対応する値を返す。 キーが見つからなかった場合には 0 を返す *)
(* search : ('a, int) tree_t -> 'a -> int *)
       
let rec search tree key =
  match tree with
  |Empty -> 0
  |Node(l,a,b,r) -> if a=key then b else if search l key = 0 then search r key else search l key


let test7 = search t4 "h" = 8
let test8 = search t5 "r" = 18
let test9 = search t6 "q" = 17
            
