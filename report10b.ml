type tree_t = Empty | Node of tree_t * int * tree_t

let double n = n*2
let pow n = n*n
let mod2 n = n mod 2

let t1 = Node(Empty,5,Node(Empty,6,Empty))
let t2 = Node(Node(Empty,9,Empty),5,Node(Empty,6,Empty))
    let t3 = Node(Node(Empty,9,Empty),5,Node(Empty,6,Node(Empty,2,Empty)))

(* 問4 tree_t型の木と関数fを受け取ったら、木の中のNodeに入っている全ての要素に対してfを施したような木を返す *)
 (* map_tree : tree_t -> (int -> int) -> tree_t *)
let rec map_tree tree f =
  match tree with
  |Empty -> Empty
  |Node(l,n,r) -> Node(map_tree l f, f n, map_tree r f)


let rec double_tree tree = map_tree tree double
let pow_tree tree = map_tree tree pow
let mod2_tree tree = map_tree tree mod2

let test1 = double_tree t1 = Node (Empty, 10, Node (Empty, 12, Empty))
let test2 = pow_tree t1 = Node (Empty, 25, Node (Empty, 36, Empty))
let test3 = mod2_tree t1 = Node (Empty, 1, Node (Empty, 0, Empty))
    


(* 問5 tree_t型の木と関数f、値initを受け取ったら、木の中のNodeをfに置き換え、Emptyをinit に置き換える *)
(* fold_tree : tree_t -> ('a -> int -> 'a -> 'a) -> 'a -> 'a  *)


let rec fold_tree tree f init =
  match tree with
  |Empty -> init
  |Node(l,n,r) -> f (fold_tree l f init) n (fold_tree r f init) 


let sum_tree tree =
  fold_tree tree (fun left num right -> left + num + right) 0

let test4 = sum_tree t1 = 11
let test5 = sum_tree t2 = 20
let test6 = sum_tree t3 = 22   
