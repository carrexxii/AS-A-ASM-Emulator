module Main

open System.IO

[<EntryPoint>]
let main args =
    printfn $"{args}"
    let path = "./programs/test.asaasm"
    try
        use file = File.OpenText path
        Parser.parse file
        |> VM.run
    with
        | :? DirectoryNotFoundException -> failwith $"Directory '{path}' not found"
        | exn -> failwith $"Unmatched exception: {exn}"
    0
