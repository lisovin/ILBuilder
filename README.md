ILBuilder
=========

MSIL computation expression builder for F# implemented using IKVM.Reflection API.

Allows to generate assembly by this:

``` fsharp
open ILBuilder

open ReflectionProvider
type R = Reflected<Assemblies = "mscorlib;ILBuilder">

// Create assembly MyAssembly.dll
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
                do! IL ret
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
```
