#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"
#load "ILExtensions.fs"

open System
open ILBuilder

assembly {
    yield! IL.publicType "Foo" {
        let! cons = IL.publicDefaultConstructor
        let! thisType = IL.thisType
        printfn "--->thisType: %A" thisType

        yield! IL.publicStaticMethod(thisType, "GetInstance") {
            do! IL.newobj cons
            do! IL.ret
        }
    }
} |> saveAssembly @"c:\temp\MyAssembly.dll"

#r @"c:\temp\MyAssembly.dll"
Foo.GetInstance()
