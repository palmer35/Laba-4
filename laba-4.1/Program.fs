open System

type BinaryTree =
    | Node of float * BinaryTree * BinaryTree 
    | Empty

let rec depth tree =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (depth left) (depth right)

let rec add (tree: BinaryTree) (value: float): BinaryTree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(data, left, right) ->
        if value < data then
            Node(data, add left value, right) 
        else
            Node(data, left, add right value)  

let rec createTree (): BinaryTree =
    printfn "Введите корень дерева (вещественное число или 'q' для выхода):"
    printf "> "
    let input = Console.ReadLine()
    if input = "q" then
        Empty
    else
        match Double.TryParse(input) with
        | true, value ->
            let tree = add Empty value
            printfn "Введите элементы дерева (вещественные числа или 'q' для выхода):"
            let rec loop (currentTree: BinaryTree): BinaryTree =
                printf "> "
                let input = Console.ReadLine()
                if input = "q" then
                    currentTree
                else
                    match Double.TryParse(input) with
                    | true, value ->
                        let newTree = add currentTree value
                        loop newTree
                    | false, _ ->
                        printfn "Некорректный ввод. Пожалуйста, введите число или 'q'."
                        loop currentTree
            loop tree
        | false, _ ->
            printfn "Некорректный ввод. Пожалуйста, введите число или 'q'."
            createTree()

let rec TreeMap (tree: BinaryTree) (f: float -> float): BinaryTree =
    match tree with
    | Node(data, left, right) ->
        let newData = f data
        Node(newData, TreeMap left f, TreeMap right f)
    | Empty -> Empty

let roundValue (x: float) = Math.Round(x)

let rec printTreeInOrder tree =
    match tree with
    | Node(data, left, right) ->
        printTreeInOrder left
        printfn "%.1f" data
        printTreeInOrder right
    | Empty -> ()

let rec printTreeClassic (tree: BinaryTree) (prefix: string) (isLeft: bool) =
    match tree with
    | Node(value, left, right) ->

        if right <> Empty then
            printTreeClassic right (prefix + (if isLeft then "│   " else "    ")) false

        printfn "%s%s%.1f" prefix (if isLeft then "└── " else "┌── ") value

        if left <> Empty then
            printTreeClassic left (prefix + (if isLeft then "    " else "│   ")) true
    | Empty -> ()


[<EntryPoint>]
let main _ =
    printfn "Создание дерева:"
    let binTree = createTree()

    printfn "\nВаше дерево (в виде дерева):"
    printTreeClassic  binTree "" true  

    printfn "\nВаше дерево (in-order обход):"
    printTreeInOrder binTree

    printfn "\nДерево после округления элементов:"
    let roundedTree = TreeMap binTree roundValue
    printTreeClassic roundedTree "" true  
    0
