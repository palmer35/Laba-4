open System

// Определение бинарного дерева
type BinaryTree =
    | Node of float * BinaryTree * BinaryTree 
    | Empty

// Функция для вычисления глубины дерева
let rec depth tree =
    match tree with
    | Empty -> 0
    | Node(_, left, right) -> 1 + max (depth left) (depth right)

// Функция для добавления элемента в дерево
let rec add (tree: BinaryTree) (value: float): BinaryTree =
    match tree with
    | Empty -> Node(value, Empty, Empty)
    | Node(data, left, right) ->
        let leftDepth = depth left
        let rightDepth = depth right
        if leftDepth <= rightDepth then
            Node(data, add left value, right)
        else
            Node(data, left, add right value)

// Функция для формирования корня дерева и добавления элементов
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

// Функция для преобразования дерева (map)
let rec TreeMap (tree: BinaryTree) (f: float -> float): BinaryTree =
    match tree with
    | Node(data, left, right) ->
        let newData = f data
        Node(newData, TreeMap left f, TreeMap right f)
    | Empty -> Empty

// Функция для округления вещественных чисел до ближайшего целого
let roundValue (x: float) = Math.Round(x)

// Обход дерева в глубину (in-order) и вывод значений
let rec printTreeInOrder tree =
    match tree with
    | Node(data, left, right) ->
        printTreeInOrder left
        printfn "%.1f" data
        printTreeInOrder right
    | Empty -> ()

// Вывод дерева в виде дерева
let rec printTreeVisual (tree: BinaryTree) (indent: string) (isLast: bool) =
    match tree with
    | Node(data, left, right) ->
        printfn "%s%s%.1f" indent (if isLast then "└── " else "├── ") data

        printTreeVisual left (indent + (if isLast then "    " else "│   ")) (right = Empty)

        printTreeVisual right (indent + (if isLast then "    " else "│   ")) true
    | Empty -> ()

[<EntryPoint>]
let main _ =
    printfn "Создание дерева:"
    let binTree = createTree()

    printfn "\nВаше дерево (в виде дерева):"
    printTreeVisual binTree "" true

    printfn "\nВаше дерево (in-order обход):"
    printTreeInOrder binTree

    printfn "\nДерево после округления элементов:"
    let roundedTree = TreeMap binTree roundValue
    printTreeVisual roundedTree "" true

    0