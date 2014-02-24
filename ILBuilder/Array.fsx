//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

(*
let createArray n = 
    il {
        for n in 0..n do
            dup
            ldc_i4 n
            ldc_i4 n
            stelem_i4
    }
*)
let initArray<'TElement> n (f : int -> 'TElement) =
    let ld, stelem = 
        match typeof<'TElement> with
        | ty when ty = typeof<bool> -> 
            (fun i -> IL.ldc_bool (f i |> box |> unbox)),
            IL.stelem_i
        | ty when ty = typeof<int32> -> 
            (fun i -> IL.ldc_i4 (f i |> box |> unbox)),
            IL.stelem_i
        | ty when ty = typeof<int64> -> 
            (fun i -> IL.ldc_i8 (f i |> box |> unbox)),
            IL.stelem_i
        | ty when ty = typeof<string> -> 
            (fun i -> IL.ldstr (f i |> box |> unbox)),
            IL.stelem_ref
        | ty -> 
            failwithf "Not supported type of element '%s'" ty.FullName

    il {
        let! loc = IL.declareLocal<'TElement[]>()
        do! IL.ldc_i4 n
        do! IL.newarr typeof<'TElement>
        do! IL.stloc loc
        for i in 0..n-1 do
            do! IL.ldloc loc
            do! IL.ldc_i4 i
            do! ld i
            do! stelem
        do! IL.ldloc loc    
    }

assembly {
    do! publicType ("Test") {
        do! publicDefaultEmptyConstructor

        let n = 5
        do! publicStaticMethod<string[]> "DoIt" [] {
            do! initArray<System.String> 5 (fun i -> sprintf "Foo %d" i)

            do! IL.ret
        }
    }
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
Test.DoIt()
