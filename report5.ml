#use "global.ml";;
(* 問１・・・整数のリストを受け取ったらその中の偶数のみを含むリストを返す *)
(* even : int list -> int list *)
let rec even lst = 
  match lst with
  | [] -> []
  | first :: rest -> (if first mod 2 = 0 then first :: even rest
  else even rest)

(* テスト *)
let test1 = even [0; 3; 4] = [0; 4]
let test2 = even [1; 5; 6] = [6]
let test3 = even [1; 1; 1] = []


(* 問２・・・予め昇順に並んだリストlstと数字nを受け取ったらlstを前から順に見ていき昇順になる位置にnを挿入したリストを返す *)
(* insert : int list -> int -> int list *)
let rec insert lst n =
    match lst with
    | [] -> [n]
    | first :: rest -> (if n > first then first :: insert rest n
    else n :: (first :: rest))

(* テスト *)
let test1 = insert [0; 6; 7] 8 = [0; 6; 7; 8]
let test2 = insert [0; 2; 3] 0 = [0; 0; 2; 3]
let test3 = insert [0; 6; 7] 5 = [0; 5; 6; 7]    


(* 問３・・・リストを受け取ったら挿入法により昇順に整列したリストを返す *)
(* ins_sort : int list -> int list *)
let rec ins_sort lst =
    match lst with
    | [] -> []
    | first :: rest -> insert (ins_sort rest) first

(* テスト *)
let test1 = ins_sort [3; 1; 9] = [1; 3; 9]
let test2 = ins_sort [9; 8; 7] = [7; 8; 9]
let test3 = ins_sort [0; 1; 2] = [0; 1; 2]


(* 問４・・・ekimei_t型のリストを受け取り順に整列し重複した駅を除いたekimei_t型のリストを返す *)
(* seiretsu2 : ekimei_t list -> ekimei_t list *)
let rec seiretsu2 lst =
  match lst with
  | [] -> []
  | first :: rest -> (let rec insert lst n =
  match lst with
  | [] -> [n]
  | {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: rest4 -> (match n with
  | {kanji = kanji2_v; kana = kana2_v; romaji = romaji2_v; ken = ken2_v; shozoku = shozoku2_v} -> (if kanji1 = kanji2_v && ken1 = ken2_v then lst
  else if ken2_v = ken1 then (if kanji1 < kanji2_v then {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: insert rest4 n
  else n :: lst)
  else if ken1 > ken2_v then n :: lst
  else {kanji = kanji1; kana = kana1; romaji = romaji1; ken = ken1; shozoku = shozoku1} :: insert rest4 n))
  in (let rec sort lst =
  match lst with
  | [] -> []
  | first :: rest -> insert (sort rest) first
      in sort lst))

(* テスト *)
let test1 = seiretsu2 [{kanji="代々木上原";
                        kana="よよぎうえはら";
                        romaji="yoyogiuehara";
                        ken="千葉";
                        shozoku="千代田線"};
                       {kanji="代々木上原";
                        kana="よよぎうえはら";
                        romaji="yoyogiuehara";
                        ken="東京";
                        shozoku="千代田線"};
                       {kanji="明治神宮前";
                        kana="めいじじんぐうまえ";
                        romaji="meijijingumae";
                        ken="東京";
                        shozoku="千代田線"};] = [{kanji = "代々木上原"; kana = "よよぎうえはら";
    romaji = "yoyogiuehara"; ken = "千葉"; shozoku = "千代田線"};
   {kanji = "代々木上原"; kana = "よよぎうえはら";
    romaji = "yoyogiuehara"; ken = "東京"; shozoku = "千代田線"};
   {kanji = "明治神宮前"; kana = "めいじじんぐうまえ";
    romaji = "meijijingumae"; ken = "東京"; shozoku = "千代田線"}]
let test2 = seiretsu2 [{kanji="代々木公園";
                        kana="よよぎこうえん";
                        romaji="yoyogikoen";
                        ken="東京";
                        shozoku="千代田線"};
                       {kanji="代々木上原";
                        kana="よよぎうえはら";
                        romaji="yoyogiuehara";
                        ken="千葉";
                        shozoku="千代田線"};
                       {kanji="明治神宮前";
                        kana="めいじじんぐうまえ";
                        romaji="meijijingumae";
                        ken="東京";
                        shozoku="千代田線"};] = [{kanji="代々木上原";
                                                      kana="よよぎうえはら";
                                                      romaji="yoyogiuehara";
                                                      ken="千葉";
                                                      shozoku="千代田線"};
                                                     {kanji="代々木公園";
                                                      kana="よよぎこうえん";
                                                      romaji="yoyogikoen";
                                                      ken="東京";
                                                      shozoku="千代田線"};
                                                     {kanji="明治神宮前";
                                                      kana="めいじじんぐうまえ";
                                                      romaji="meijijingumae";
                                                      ken="東京";
                                                      shozoku="千代田線"};]
let test3 = seiretsu2 [{kanji="a";
                        kana="a";
                        romaji="a";
                        ken="東京";
                        shozoku="千代田線"};
                       {kanji="b";
                        kana="b";
                        romaji="b";
                        ken="千葉";
                        shozoku="千代田線"};
                       {kanji="a";
                        kana="a";
                        romaji="a";
                        ken="神奈川";
                        shozoku="千代田線"};] = [{kanji = "b"; kana = "b"; romaji = "b"; ken = "千葉";
    shozoku = "千代田線"};
   {kanji = "a"; kana = "a"; romaji = "a"; ken = "東京";
    shozoku = "千代田線"};
   {kanji = "a"; kana = "a"; romaji = "a"; ken = "神奈川";
    shozoku = "千代田線"}]
 (* let test4 = seiretsu2 global_ekimei_listn *)           
    
