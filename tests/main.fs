module Main

open System.IO

open ASAEmulator

[<EntryPoint>]
let main args =
    printfn $"{args}"
    let path = "./programs/test.asaasm"
    try
        let lines = File.ReadAllLines path
        let state = Parser.parse lines |> VM.load (fun str -> printfn $"## {str}")
        printfn $"{state}"
        VM.run state
    with
        | :? DirectoryNotFoundException -> failwith $"Directory '{path}' not found"
        | exn -> failwith $"Unmatched exception: {exn}"
    0
