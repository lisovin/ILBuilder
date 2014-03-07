//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"
#load "ILExtensions.fs"

open System
open ILBuilder

assembly {
    yield! IL.publicType ("Test", typeof<IDisposable>) {
        yield! IL.overrideMethod (typeof<IDisposable>.GetMethod("Dispose")) {
            do! IL.ilprintf "--->Dispose!"
        }
    }
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
let foo () = 
    use t = new Test()
    printfn "--->using"

foo()