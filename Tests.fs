module Tests

open System
open Xunit


 
type Tree =
  | Empty 
  | Node of Tree * int * Tree

let largeTree =
  [ 1 .. 10000000 ]
  |> List.fold (fun (t: Tree) i -> Node(Empty, i, t) ) Empty 
//  |> List.fold (fun (t: Tree) i -> Node(t, i, t) ) Empty 

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

// http://fssnip.net/7VQ/title/Continuation-monad-for-mortals
type Cont<'T, 'U> = ('T -> 'U)

/// An incomplete computation is a function which, when given a continuation,
/// will return a value.
type Inc<'T, 'U> = Cont<'T, 'U> -> 'U

/// Creates an incomplete computation that holds the given value.
let ret (t : 'T) : Inc<'T, _> =
    fun (cont : Cont<'T, _>) -> cont t

/// Composition of incomplete computations.
let bind (incT : Inc<'T, _>) (wrap : 'T -> Inc<'U, _>) : Inc<'U, _> =
    fun (contU : Cont<'U, _>) ->   // return an Inc, which is a function that takes a continuation as input
        incT (fun t ->             // force the given incomplete computation to cough up its wrapped value
            (wrap t) contU)        // re-wrap the raw value so it can be sent to the given continuation

/// Monad definition.
type ContinuationBuilder() =
    member __.Return(value) = ret value
    member __.Bind(inc, wrap) = bind inc wrap

/// Builder instance.
let continuation = ContinuationBuilder()

let rec invertTree2 t: Inc<Tree, _>= 
  continuation {
    match t with
      | Node (t1,tall,t2) ->
          let! t1inv = invertTree2 t1
          let! t2inv = invertTree2 t2
          return Node (t2inv, tall, t1inv)
      | Empty -> return Empty 
  }

// trading stack space for heap space
let invertTree (t: Tree): Tree =
  let rec invertTreeInner (t:Tree) (cont: Tree -> Tree ) =
    match t with
      | Node (t1,tall,t2) ->
          invertTreeInner t1 (fun left -> 
              invertTreeInner t2 (fun right ->
              cont (Node (right, tall, left))))
      | Empty -> cont Empty 
  invertTreeInner t id
[<Fact>]
let ``My test`` () =
//    print fn "%A" largeTree 
//    printfn "%A" (nodes largeTree) 
    let doubleflip = invertTree (invertTree largeTree) 
    printfn "ok"
//    printfn "%A" (sum largeTree) 
    printfn "ok2"
    Assert.Equal(largeTree, doubleflip)
    let foo1 = invertTree largeTree
    let foo2 = invertTree largeTree
    Assert.Equal(invertTree foo1, invertTree foo2)
    Assert.Equal(foo1, foo2)
 //   Assert.Equal(largeTree, doubleflip2)
