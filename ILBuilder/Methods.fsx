//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder
open IKVM.Reflection
open IKVM.Reflection.Emit
open Xyz

assembly {
    do! IL.extensionsType "Test" {
        yield! IL.extensionMethod (ClrType typeof<int>) "Foo" [ ClrType typeof<string> ] {
            do! IL.ldc_i4_1
            do! IL.ret
        }
    }
} |> saveAssembly @"c:\temp\test.dll"

"asdf".DoIt()
"asdf".Foo()

#r @"c:\temp\test.dll"
Test.Foo("asdf")
open Test
let t = Test()
t.MyProp
