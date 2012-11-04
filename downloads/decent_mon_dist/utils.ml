open List

let make_list (size:int) (fill_function:int -> 'a):'a list=
	let rec list_of_indexes n = if n=1 then [1] else (list_of_indexes (n-1))@[n] in
		map fill_function (list_of_indexes size)
		
let map3 (f:'a -> 'b -> 'c -> 'd) (a:'a list) (b:'b list) (c:'c list) =
	map2 (fun x (y1,y2) -> f x y1 y2) a (map2 (fun x y -> (x,y)) b c)

let iter3 (f:'a -> 'b -> 'c -> unit) (a:'a list) (b:'b list) (c:'c list) =
	iter2 (fun x (y1,y2) -> f x y1 y2) a (map2 (fun x y -> (x,y)) b c)
;;


let rec prefix (n:int) (l:'a list) =
	let rec prefix_rec (n:int) (l:'a list) (r:'a list) =
		if (n=0) then r
		else
			match l with
				[] -> failwith "erreur argument l"
				| head::remainder -> 
			prefix_rec (n-1) remainder (r @ [head])
	in 
		prefix_rec n l []