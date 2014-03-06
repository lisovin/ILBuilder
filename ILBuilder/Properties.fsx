//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder
open IKVM.Reflection
open IKVM.Reflection.Emit

assembly {
    do! IL.publicType "Test" {
        yield! IL.publicMethod<int> "Foo" [] {
            do! IL.ldc_i4_3
            do! IL.ret
        }
    }
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
let t = Test()
t.MyProp
