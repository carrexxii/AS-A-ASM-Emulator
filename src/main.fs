﻿module Main

open System.IO

[<EntryPoint>]
let main args =
    printfn $"{args}"
    let path = "./programs/test.asaasm"
    try
        use file = File.OpenText path
        let state = Parser.parse file |> VM.load (fun str -> printfn $"## {str}")
        VM.run state
    with
        | :? DirectoryNotFoundException -> failwith $"Directory '{path}' not found"
        | exn -> failwith $"Unmatched exception: {exn}"
    0
