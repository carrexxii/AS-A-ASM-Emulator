module Main

open System.IO

open ASAEmulator

[<EntryPoint>]
let main args =
    printfn $"{args}"
    let path = "./programs/test.asaasm"
    try
        use file = File.OpenText path
        let state = Parser.parseFile file |> VM.load (fun str -> printfn $"## {str}")
        VM.run state
    with
        | :? DirectoryNotFoundException -> failwith $"Directory '{path}' not found"
        | exn -> failwith $"Unmatched exception: {exn}"
    0
