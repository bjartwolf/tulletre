module Tests

open System
open Xunit


 
type Tree =
  | Empty 
  | Node of Tree * int * Tree

let root =
  [ 1 .. 10000 ]
  |> List.fold (fun (t: Tree) i -> Node(t, i, t) ) Empty 

 
let rec flip (t: Tree) : Tree =
    match t with 
        | Empty -> Empty
        | Node (t1,num,t2) -> Node (flip t2, num, flip t1)

let rec sum (t: Tree) : int =
    match t with 
        | Empty -> 0 
        | Node (t1,num,t2) -> num + sum(t1) + sum(t2) 


let tre = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)



[<Fact>]
let ``My test`` () =
    printfn "%A" (sum root) 
    let doubleflip = flip (flip tre)
    Assert.Equal(tre, doubleflip)
