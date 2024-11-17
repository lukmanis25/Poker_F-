let SHOW x = printf "%A\n" x
type Tree =
    | Empty
    | Node of int * Tree * Tree

//#### Binary Tree as DU

let tree =
    Node (8,
         Node(3, Node(1, Empty, Empty), Node(6, Node(4, Empty, Empty), Node(7, Empty, Empty))),
         Node(10, Empty, Node(14, Node(13, Empty, Empty), Empty)))

//### Exercise 1.4
//##Insert element into Binary Search Tree

//#### --------------- Your code goes below --------------- *)
let rec insertBST (value: int) (tree: Tree): Tree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
        if value < v then
            Node(v, insertBST value left, right)
        elif value > v then
            Node(v, left, insertBST value right)
        else
            tree 


let ``exercise 1.4 raw`` = insertBST 5 tree 
//** #### Value of ``exercise 1.4`` *)
SHOW ``exercise 1.4 raw``

//### Exercise 3.1
//##Implement `parseScore`.

//#### --------------- Your code goes below --------------- *)
let (|Digit|_|) char =
    let zero = System.Convert.ToInt32 '0'
    if System.Char.IsDigit char then Some(System.Convert.ToInt32 char - zero) else None

let (|Strike|Spare|Miss|Digit|Invalid|) char =
    match char with
    | 'X' -> Strike
    | '/' -> Spare
    | '-' -> Miss
    | Digit digit -> Digit digit
    | _ -> Invalid

let rec parseScore (chars: char list): int option list =
    let rec helper prevScore chars =
        match chars, prevScore with
        | [], _ -> []
        | Strike :: tail, _ -> Some 10 :: helper None tail
        | Spare :: tail, Some p -> Some (10 - p) :: helper None tail
        | Miss :: tail, Some p -> Some 0 :: helper None tail
        | Miss :: tail, None -> Some 0 :: helper (Some 0) tail
        | Digit d :: tail, Some p -> Some d :: helper None tail
        | Digit d :: tail, None -> Some d :: helper (Some d) tail
        | Invalid :: tail, _  //Invalid iputs
        | Spare :: tail, None 
          -> None :: helper None tail
    helper None chars

let ``exercise 3.1`` =
    parseScore [ 'X'
                 '4'
                 '/'
                 '2'
                 '-'
                 'N' ]
//** #### Value of ``exercise 3.1`` *)
SHOW ``exercise 3.1``

//### Exercise 3.2
//##Implement `countScore`

//#### --------------- Your code goes below --------------- *)
let rec countScore (scores: int list): int =
    let rec helper scores frameCount =
        match scores, frameCount with
        | [], _ -> 0
        | _, 10 -> 0
        | x :: y :: z :: rest, _ when x = 10 -> // Strike
            10 + y + z + helper (y :: z :: rest) (frameCount + 1)
        | x :: y :: z :: rest, _ when x + y = 10 -> // Spare
            10 + z + helper (z :: rest) (frameCount + 1)
        | x :: y :: rest, _ -> // Normal frame
            x + y + helper rest (frameCount + 1)
        | _ -> 0
    helper scores 0

let ``exercise 3.2`` =
    [ [ 10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10
        10 ]
      [ 9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0
        9
        0 ]
      [ 5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5
        5 ]
      [ 10
        9
        1
        5
        5
        7
        2
        10
        10
        10
        9
        0
        8
        2
        9
        1
        10 ] ]
    |> List.map countScore
//** #### Value of ``exercise 3.2`` *)
SHOW ``exercise 3.2``

//### sequenceOpts function *)
let sequenceOpts (optionals: 'a option list): 'a list option =
    let rec sequence' acc optionals =
        match optionals, acc with
        | [], _ -> Option.map List.rev acc
        | Some h :: t, Some acc -> sequence' (Some(h :: acc)) t
        | _ -> None

    sequence' (Some []) optionals


//### Homework 1
//##Implement `bowlingScore`.

//###Hint: Use `sequenceOpts` to convert from list of options to option of list
let bowlingScore (score: string): int option =
    let chars = score.ToCharArray() |> List.ofArray
    let parsedScores = parseScore chars
    match sequenceOpts parsedScores with
    | Some scores -> Some(countScore scores)
    | None -> None

let ``homework 1`` =
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X"
      "X44/2-" ]
    |> List.map bowlingScore

//** #### Value of ``homework 1`` *)
SHOW ``homework 1``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]


//### Homework 2
//###Write new, **tail-recursive** versions of `parseScore` and `countScore`.
//###Implement `bowlingScoreTail` to use those 2 new functions


let rec parseScoreTail (chars: char list) (acc: int option list): int option list =
    let rec helper prevScore chars acc =
        match chars, prevScore with
        | [], _ -> List.rev acc
        | Strike :: tail, _ -> helper None tail (Some 10 :: acc)
        | Spare :: tail, Some p -> helper None tail (Some (10 - p) :: acc)
        | Miss :: tail, Some p -> helper None tail (Some 0 :: acc)
        | Miss :: tail, None -> helper (Some 0) tail (Some 0 :: acc)
        | Digit d :: tail, Some p -> helper None tail (Some d :: acc)
        | Digit d :: tail, None -> helper (Some d) tail (Some d :: acc)
        | Invalid :: tail, _  //Invalid iputs
        | Spare :: tail, None 
          -> helper None tail (None :: acc)
    helper None chars acc
    
let rec countScoreTail (scores: int list) (acc: int): int =
    let rec helper scores acc frameCount =
        match scores, frameCount with
        | [], _ -> acc
        | _, 10 -> acc
        | x :: y :: z :: rest, _ when x = 10 -> // Strike
            helper (y :: z :: rest) (acc + 10 + y + z) (frameCount + 1)
        | x :: y :: z :: rest, _ when x + y = 10 -> // Spare
            helper (z :: rest) (acc + 10 + z) (frameCount + 1)
        | x :: y :: rest, _ -> // Normal frame
            helper rest (acc + x + y) (frameCount + 1)
        | _ -> acc
    helper scores acc 0

let bowlingScoreTail (score: string): int option =
    let chars = score.ToCharArray() |> List.ofArray
    match parseScoreTail chars [] |> sequenceOpts with
    | Some scores -> Some(countScoreTail scores 0)
    | None -> None

let ``homework 2`` =     
    [ "XXXXXXXXXXXX"
      "9-9-9-9-9-9-9-9-9-9-"
      "9--/9-9-9-9-9-9-9-9-"
      "X-/9-9-9-9-9-9-9-9-"
      "9-X9-9-X--9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-9-"
      "9-9-9-9-9-9-9-9-9-XXX"
      "5/5/5/5/5/5/5/5/5/5/5"
      "5/5/5/5/5/5/5/5/5/5/X"
      "X9/5/72XXX9-8/9/X"
      "X44/2-" ]
    |> List.map bowlingScoreTail 
//** #### Value of ``homework 2`` *)
SHOW ``homework 2``

//EXPECTED RESULTS   
//[Some 300; Some 90; Some 100; Some 111; Some 92;
//   Some 90; Some 111; Some 150; Some 155; Some 187]

//////////////////////////////////////////////////////////////
/// Indeks: 184306
/// Imię: Łukasz
/// Nazwisko: Smoliński
/// 
/// Podsumowanie zalizowanych zadan:
/// 1.4 zrobione
/// 3.x zrobione
/// homework1 zrobione
/// homework2 zrobione
/// dodatkowe nie zrobione