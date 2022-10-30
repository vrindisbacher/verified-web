
type 'a kd_tree = 
| Leaf 
| Node of 'a * 'a kd_tree * 'a kd_tree 


class kd_persist (d : int) = object(self) 
    val mutable tree : float list kd_tree = Leaf 
    val dim : int = d

    method kd_insert (k : float list) = 
        (* inserts k into kd_tree t *)
        (* alternates on axis at each depth *)
        let rec ins (t : float list kd_tree) (axis : int) =
            match t with 
            | Leaf -> Node (k, Leaf, Leaf)  
            | Node (k', l_tree, r_tree) -> 
                let i = axis mod dim in 
                if List.nth k i < List.nth k' i then 
                    Node (k', (ins l_tree (axis + 1)), r_tree) 
                else 
                    Node (k', l_tree, (ins r_tree (axis + 1)))
        in if List.length k = dim then tree <- ins tree 0 

    method sort_and_insert_top_n (k : float list) (ideal_node : float list) (f : float list -> float list -> float) (acc : float list list) (n : int) = 
        let comp x y = 
            let x_score = f ideal_node x in
            let y_score = f ideal_node y in 
            if x_score = y_score then 0 else if x_score < y_score then 1 else -1
        in 
        let replace l idx elem = List.mapi (fun i x -> if i = idx then elem else x) l
        in 
        if List.length acc < n then 
            (List.sort comp (k :: acc))
        else 
            if f ideal_node (List.nth acc (n - 1)) < f ideal_node k then 
                List.sort comp (replace acc (n - 1) k) 
            else 
                acc 

    method kd_knn (ideal_node : float list) (f : float list -> float list -> float) (n : int) = 
        let rec aux t (axis : int) (acc : float list list) =  
            match t with 
            | Leaf -> acc 
            | Node (k', l_tree, r_tree) -> 
                let i = axis mod dim in 
                if List.nth ideal_node i < List.nth k' i then 
                    aux l_tree (axis + 1) (self#sort_and_insert_top_n k' ideal_node f acc n) 
                else 
                    aux r_tree (axis + 1) (self#sort_and_insert_top_n k' ideal_node f acc n) 
        in aux tree 0 []

    method kd_find (default : float list) (k : float list) = 
        let rec find (t : float list kd_tree) (axis : int) = 
            match t with 
            | Leaf -> default 
            | Node (k', l_tree, r_tree) -> 
                let i = axis mod dim in 
                if k = k' then 
                    k'
                else if List.nth k i < List.nth k' i then 
                    find l_tree (axis + 1)
                else 
                    find r_tree (axis + 1)
            in find tree 0 
end 
