open System
open Xunit
 
type Tree =
  | Empty 
  | Node of Tree * Tree

let largeTree =
  [ 1 .. 10000000 ]
  |> List.fold (fun (t: Tree) i -> Node(Empty, t) ) Empty 
//  |> List.fold (fun (t: Tree) i -> Node(t, i, t) ) Empty 

let rec invertTree (t:Tree) (continuation: Tree -> Tree ) =
  match t with
    | Node (l,r) ->
        invertTree l (fun left -> 
            invertTree r (fun right ->
            continuation (Node (right, left))))
    | Empty -> continuation Empty 

module Program = 
    let [<EntryPoint>] main _ = 
        let flip = invertTree largeTree id
        printfn "ok"
    //    printfn "%A" (sum largeTree) 
        printfn "ok2"
    //    Assert.Equal(largeTree, doubleflip)
        Assert.Equal(largeTree, largeTree)
        0

