module Tests

open System
open Xunit


type Tree =
  | Empty 
  | Node of Tree * int * Tree

let rec flip (t: Tree) : Tree =
    match t with 
        | Empty -> Empty
        | Node (t1,num,t2) -> Node (flip t2, num, flip t1)

let tre = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)



[<Fact>]
let ``My test`` () =
    let doubleflip = flip (flip tre)
    Assert.Equal(tre, doubleflip)
