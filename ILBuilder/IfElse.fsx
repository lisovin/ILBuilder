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
    do! publicType ("Test") {
        yield! publicMethod<int> "IfThen" [ClrType typeof<int>] {
            do! IL.ldc_i4(-1)
            do! IL.ldarg_1
            do! IL.ldc_i4_1
            do! IL.ifThen IL.bne_un_s <| il { 
                do! IL.ilprintf "--->condition is true"
                do! IL.pop
                do! IL.ldc_i4_0
            }
            do! IL.ret 
        }

        yield! publicMethod<int> "IfThenElse" [ClrType typeof<int>] {
            do! IL.ldarg_1
            do! IL.ldc_i4_0
            do! (IL.ifThenElse IL.bne_un_s <| il { 
                do! IL.ilprintf "--->condition is true"
                do! IL.ldc_i4_0 
            } <| il {
                do! IL.ilprintf "--->condition is false"
                do! IL.ldc_i4(-1)
            })
            do! IL.ret 
        }
    }
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
let t = Test()
t.IfThen(1)
t.IfThenElse(0)
