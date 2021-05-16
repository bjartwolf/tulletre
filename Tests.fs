module Tests

open System
open Xunit


 
type Tree =
  | Empty 
  | Node of Tree * int * Tree

let largeTree =
  [ 1 .. 30 ]
  |> List.fold (fun (t: Tree) i -> Node(t, i, t) ) Empty 

 
let rec flip (t: Tree) : Tree =
    match t with 
        | Empty -> Empty
        | Node (t1,num,t2) -> Node (flip t2, num, flip t1)

let rec sum (t: Tree) : int64 =
    match t with 
        | Empty -> 0L
        | Node (t1,num,t2) -> int64(num) + sum(t1) + sum(t2) 

let rec nodes (t: Tree) : int64 =
    match t with 
        | Empty -> 0L 
        | Node (t1,num,t2) -> 1L + nodes(t1) + nodes(t2) 

let tre = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)


[<Fact>]
let ``My test`` () =
//    printfn "%A" largeTree 
    printfn "%A" (nodes largeTree) 
    let doubleflip = flip (flip largeTree)
    printfn "ok"
    printfn "%A" (sum largeTree) 
    printfn "ok2"
    Assert.Equal(largeTree, doubleflip)
