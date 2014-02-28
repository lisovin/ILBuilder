#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

assembly {
    do! publicType "Foo" {
        yield! publicMethod<string> "Bar" [] {
            do! IL.ldstr "adsf"
            try
                do! IL.ldstr "qwer"
                do! IL.ldstr "-->Success"
                do! IL.call (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])) 
            finally 
                do! fun (u, il) -> () 
//            with 
  //              | ex -> do! IL.ldstr (sprintf "-->caugh exception: %A" ex)
                //fun () -> 
                //    do! IL.ldstr "-->Finally"
                //    do! IL.call (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |])) 
        }
    }
} |> saveAssembly @"c:\temp\foo.dll" 

#r @"c:\temp\foo.dll"
(Foo()).Bar()