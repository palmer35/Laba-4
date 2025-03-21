open System

// Определение бинарного дерева
type BinaryTree =
    | Node of float * BinaryTree * BinaryTree 
    | Empty

// Функция fold для дерева
let rec foldTree f acc tree =
    match tree with
    | Empty -> acc
    | Node(data, left, right) ->
        let newAcc = f acc data // Применяем функцию к текущему элементу
        let leftAcc = foldTree f newAcc left // Рекурсивно обрабатываем левое поддерево
        foldTree f leftAcc right // Рекурсивно обрабатываем правое поддерево

// Функция для подсчета количества вхождений элемента в дерево
let countOccurrences tree target =
    foldTree (fun acc value -> if value = target then acc + 1 else acc) 0 tree

// Функция для создания дерева (взята из предыдущего примера)
let rec createTree (): BinaryTree =
    printfn "Введите корень дерева (вещественное число или 'q' для выхода):"
    printf "> "
    let input = Console.ReadLine()
    if input = "q" then
        Empty
    else
        match Double.TryParse(input) with
        | true, value ->
            let tree = Node(value, Empty, Empty)
            printfn "Введите элементы дерева (вещественные числа или 'q' для выхода):"
            let rec loop (currentTree: BinaryTree): BinaryTree =
                printf "> "
                let input = Console.ReadLine()
                if input = "q" then
                    currentTree
                else
                    match Double.TryParse(input) with
                    | true, value ->
                        let newTree = Node(value, currentTree, Empty) // Простое добавление в левое поддерево
                        loop newTree
                    | false, _ ->
                        printfn "Некорректный ввод. Пожалуйста, введите число или 'q'."
                        loop currentTree
            loop tree
        | false, _ ->
            printfn "Некорректный ввод. Пожалуйста, введите число или 'q'."
            createTree()

// Основная программа
[<EntryPoint>]
let main _ =
    printfn "Создание дерева:"
    let tree = createTree()

    printfn "\nВведите элемент, количество вхождений которого нужно подсчитать:"
    printf "> "
    let input = Console.ReadLine()
    match Double.TryParse(input) with
    | true, target ->
        let count = countOccurrences tree target
        printfn "Элемент %.1f встречается в дереве %d раз(а)." target count
    | false, _ ->
        printfn "Некорректный ввод. Пожалуйста, введите число."

    0