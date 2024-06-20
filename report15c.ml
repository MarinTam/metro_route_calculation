(* 問4 整数のリストを受け取り正の値だけの合計の平均を返す.100以上または-100以下が出たら終了 *)
(* f : int list -> int *)
let f lst =
  let rec pos lst1 =
    match lst1 with
      [] -> []
    | first :: rest ->
      if first > 0
      then if first >=100 || first <= (-100)
        then [] else first :: pos rest else pos rest
  in let pos_lst = pos lst
  in let average lst1  =
      let rec sum lst2 = match lst2 with
        [] -> 0
      | first :: rest -> first + sum rest
       in let length = List.length lst1
       in let sum_n = sum lst1
       in sum_n / length
  in average pos_lst
    
let test1 = f [2; -3; 5; 9; -1; -8; 0; 7; 100; 3; -4; 9; 100; 1] = 5
let test2 = f [2; -63; 5; 9; -1; -80; 0; 79; -103; 3; -4; 9; 100; 1] = 17
let test3 = f [2; -3; 5; 5; -1; -8; 0; 7; 782; 3; -4; 9; 150; 1] = 4
            
