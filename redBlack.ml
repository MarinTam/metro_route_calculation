type color_t = Red | Black                   
  type ('a, 'b, 'c) t = Empty
		      | Node of ('a, 'b, 'c) t * 'a * 'b * color_t * ('a, 'b, 'c) t
                    
  (* 空の木 *)
  let empty = Empty
    
(* 問5 教科書227に従い赤黒木を再構成する *)
(* balance : ('a, 'b, 'c) t -> ('a, 'b, 'c) t *)

  let rec balance rb_tr = match rb_tr with
      Empty -> Empty
    | Node(Node(Node(a,xk,xv,Red,b),yk,yv,Red,c),zk,zv,Black,d)
    | Node(Node(a,xk,xv,Red,Node(b,yk,yv,Red,c)),zk,zv,Black,d)
    | Node(a,xk,xv,Black,Node(Node(b,yk,yv,Red,c),zk,zv,Red,d))
    | Node(a,xk,xv,Black,Node(b,yk,yv,Red,Node(c,zk,zv,Red,d)))
      -> Node(Node(a,xk,xv,Black,b),yk,yv,Red,Node(c,zk,zv,Black,d))
    | Node(l,k,v,col,r) -> Node(balance l,k,v,col,balance r)
                                    
(* 
  let rec balance rb_tr = match rb_tr with
      Empty -> Empty
    | Node(l,k,v,col,r) ->
     if col = Black
      then (match l with
          Empty -> (match r with
            Empty -> rb_tr
          | Node(rl,rk,rv,rcol,rr) ->
            if rcol = Red
            then (match rl with
                Empty -> (match rr with
                  Empty -> rb_tr
                | Node(rrl,rrk,rrv,rrcol,rrr) -> if rrcol = Red
                  then Node(Node(l,k,v,col,rl) ,rk,rv,rcol,Node(rrl,rrk,rrv,Black,rrr))
                  else Node(l,k,v,col,Node(rl,rk,rv,rcol,balance rr)))
              | Node(rll,rlk,rlv,rlcol,rlr) -> if rlcol = Red
                then Node(Node(l,k,v,col,rll) ,rlk,rlv,rlcol,Node(rlr,rk,rv,Black,rr))
                else Node(l,k,v,col,Node(balance rl,rk,rv,rcol,rr)))
            else Node(l,k,v,col,balance r))
          | Node(ll,lk,lv,lcol,lr) ->
            (if lcol = Red then (match ll with
              Empty -> (match lr with
                Empty -> rb_tr
              | Node(lrl,lrk,lrv,lrcol,lrr) ->
                if lrcol = Red
                then Node(Node(ll,lk,lv,Black,lrl),lrk,lrv,lrcol,Node(lrr,k,v,col,r))
                else Node(Node(ll,lk,lv,lcol,balance lr),k,v,col,r))
            | Node(lll,llk,llv,llcol,llr) ->
              if llcol = Red
              then Node(Node(lll,llk,llv,Black,llr),lk,lv,lcol,Node(lr,k,v,col,r))
              else Node(Node(balance ll, lk,lv,lcol,lr),k,v,col,r))
          else Node(balance l, k,v,col,r)))
      else Node(balance l,k,v,col, balance r)
*)                 
              
  (* 問6 赤黒木とキーと値を受け取ったら、それを挿入した赤黒木を返す *)
          (* insert : ('a, 'b, 'c) t -> 'a -> 'b -> ('a, 'b, 'c) t *)
  let rec insert rb_tr k v = match rb_tr with
      Empty -> Node(Empty,k,v,Red,Empty)
    | Node(l,key,value,col,r) ->
      if k = key
      then Node(l,k,v,col,r)
      else if k < key
      then Node(insert l k v, key,value,col,r)
      else Node(l,key,value,col,insert r k v)

  (* 問7 赤黒木とキーを受け取ったら、そのキーを持つノードに保存されている値を返す *)
         (* search : ('a, 'b, 'c) t -> 'a -> 'b *)
  let rec search rb_tr k = match rb_tr with
      Empty -> raise Not_found
    | Node(l,key,value,col,r) ->
      if k = key then value
      else if k < key then search l k
      else search r k
          
let rec fold f init tree = match tree with
    Empty -> init
  | Node (left, key, value, color, right) ->
      f (fold f init left) key value (fold f init right)

let height tree = fold (fun left _ _ right -> 1 + max left right) 0 tree

let length tree = fold (fun left _ _ right -> left + right + 1) 0 tree
