module Tests

open System
open Xunit
open FsUnit 

 
type Tree =
  | Empty 
  | Node of Tree * int * Tree

let largeTree =
  [ 1 .. 10000000 ]
//  |> List.fold (fun (t: Tree) i -> Node(t, i, Empty) ) Empty 
  |> List.fold (fun (t: Tree) i -> Node(Empty, i, t) ) Empty 

let rec flip (t: Tree) : Tree =
    match t with 
        | Empty -> Empty
        | Node (t1,num,t2) -> Node (flip t2, num, flip t1)

// TODO REFACOTR
let rec sum (t: Tree) : int64 =
    match t with 
        | Empty -> 0L
        | Node (t1,num,t2) -> int64(num) + sum(t1) + sum(t2) 

// TODO REFACOTR
let rec nodes (t: Tree) : int64 =
    match t with 
        | Empty -> 0L 
        | Node (t1,num,t2) -> 1L + nodes(t1) + nodes(t2) 

 
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

let treeEqual (trees: Tree*Tree) : bool =
  let rec treeEqualInner (t1, t2) (cont: bool -> bool) = 
      match (t1,t2) with
        | Empty, Empty -> cont true 
        | Node (_,_,_), Empty  -> cont false 
        | Empty, Node (_,_,_) -> cont false 
        | Node (t1l,tall1,t1r), Node(t2l, tall2, t2r)  ->
            treeEqualInner (t1l, t2l) (fun leftEqual ->
              treeEqualInner (t1r, t2r) ( fun rightEqual -> 
                  cont (tall1 = tall2 && leftEqual && rightEqual)))
  treeEqualInner trees id 

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
    member __.Delay(mk) = fun c -> mk () c 

/// Builder instance.
let continuation = ContinuationBuilder()

let treeEqualM (trees: Tree*Tree) : bool =
  let rec treeEqualInnerM (t1, t2)  = 
    continuation {
      match (t1,t2) with
        | Empty, Empty -> return true 
        | Node (_,_,_), Empty  -> return false 
        | Empty, Node (_,_,_) -> return false 
        | Node (t1l,tall1,t1r), Node(t2l, tall2, t2r)  ->
            let! leftEq = treeEqualInnerM (t1l, t2l)
            let! rightEq = treeEqualInnerM (t1r, t2r)
            return leftEq && rightEq && tall1 = tall2 
    }
  treeEqualInnerM trees id

let invertTreeM (t: Tree): Tree =
  let rec invertTreeInner (t:Tree) = 
    continuation {
      match t with
        | Node (t1,tall,t2) ->
            let! inv1 = invertTreeInner t1 
            let! inv2 = invertTreeInner t2 
            return Node (inv2, tall, inv1)
        | Empty -> return Empty 
    }
  invertTreeInner t id

let pure' x = fun () -> x  
let apply f x = fun () -> 
                  let value = x()
                  let myFunc = f()
                  myFunc value

let map' fn = apply (pure' fn)


let bind' (f: ('a -> (unit -> 'b))): (unit -> 'a) -> (unit -> 'b) = 
  fun (foo: (unit -> 'a)) ->
    let a = foo()
    f a

// How would one write this...

#if DEBUG
[<Fact>]
let ``Assert not debug mode`` () =
  failwith "DEBUGMODE DOES NOT WORK, the compiler does not tailcall properly and it stack overflows, use --configuration Release" 
#endif

[<Fact>]
let ``Double flipeed are equal to themselves`` () =
    Assert.True(treeEqual (largeTree, largeTree |> invertTree |> invertTree))

[<Fact>]
let ``Half flipeed are not equal to themselves`` () =
    Assert.False(treeEqual (largeTree, largeTree |> invertTree))

[<Fact>]
let ``Equal trees are equal`` () =
  let tre1 = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)
  let tre2 = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)
  Assert.True(treeEqual (tre1, tre2)) 

[<Fact>]
let ``Different trees are not equal`` () =
  let tre1 = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)
  let tre2 = Node (Node (Empty,3, Node(Empty, 1, Empty)), 2, Empty)
  Assert.False(treeEqual (tre1, tre2)) 
  let tre3 = Node (Node (Empty,3, Node(Empty, 3, Empty)), 2, Empty)
  let tre4 = Node (Node (Node(Empty, 3, Empty), 3, Empty), 2, Empty)
  Assert.False(treeEqual (tre3, tre4)) 

[<Fact>]
let ``Double flipeed are equal to themselves even with monadic equality `` () =
    Assert.True(treeEqualM (largeTree, largeTree |> invertTree |> invertTree))

[<Fact>]
let ``Flips are equal`` () =
    Assert.True(treeEqualM (largeTree |> invertTree, largeTree |> invertTreeM ))




(*
[<Fact>]
let ``Normal equality crashes`` () =
  Assert.Throws<System.Exception>(
    (fun () -> (largeTree = (largeTree |> invertTree |> invertTree)) |> ignore))
*)