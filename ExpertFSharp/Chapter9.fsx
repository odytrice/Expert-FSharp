type Tree = 
    | Node of string * Tree * Tree
    | Tip of string


let tree = Node("Letters", Node("Vowels", Tip("A"), Tip("E")), Node("Consonants", Tip("K"), Tip("C")))

let build count tree =
    
    let rec loop tree i =
        if i <= count
        then loop (Node(i.ToString(),tree, tree)) (i + 1) 
        else tree

     
    loop tree 1


let rec sizeNotTailRecursive tree = 
    match tree with
    | Tip _ -> 1
    | Node(_, treeLeft, treeRight) -> sizeNotTailRecursive treeLeft + sizeNotTailRecursive treeRight

let rec sizeCont tree cont =
    match tree with
    | Tip _ -> cont 1
    | Node(_, treeLeft, treeRight) -> 
        sizeCont treeLeft (fun leftSize -> 
            sizeCont treeRight (fun rightSize -> 
                cont (leftSize + rightSize)))

tree 
|> build 10
|> sizeCont <| id