#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

assembly {
    do! publicType "Foo" {
        let! cons = publicDefaultEmptyConstructor

        yield! publicStaticMethodOfType ThisType "GetInstance" [] {
            do! IL.newobj cons
            do! IL.ret
        }
    }
} |> saveAssembly @"c:\temp\MyAssembly.dll"

#r @"c:\temp\MyAssembly.dll"
Foo.GetInstance()
