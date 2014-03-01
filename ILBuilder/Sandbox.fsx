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

assembly {
    // Create 2 types "Test_0" and "Test_1"
    for n in [0..1] do
        do! publicType (sprintf "Test_%d" n) {
            // Default empty constructor
            yield! publicDefaultEmptyConstructor
        
            // Property "MyProp" with autoimplemented getter and setter
            yield! publicAutoProperty<string> "MyProp" { get; set }
            
            // Public method "GetFoo" that would simply return "Bar"
            yield! publicMethod<string> "GetFoo" [] {
                do! IL.ldstr "Bar"
                do! IL.ret
            }
            
            // Public void method "DoiIt" that would print hash code of String type
            yield! publicVoidMethod "DoIt" [] {
                do! IL.ldtoken typeof<string>
                do! IL.call R.System.Type.``GetTypeFromHandle : System.RuntimeTypeHandle -> System.Type``
                do! IL.callvirt R.System.Type.``GetHashCode : unit -> int``
                do! IL.call R.System.Console.``WriteLine : int -> unit``
                do! IL.ret
            }
            
            // More properties "Prop_0", "Prop_1", "Prop_2" with auto getter and setter
            for n in [0..2] do
                yield! publicAutoProperty<string> (sprintf "Prop_%d" n) { get; set }
        }
} |> saveAssembly  @"c:\temp\MyAssembly.dll"

(*
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
*)