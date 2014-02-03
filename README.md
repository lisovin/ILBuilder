ILBuilder
=========

MSIL computation expression builder for F# implemented using IKVM.Reflection API.

Allows to generate assembly by this:

``` fsharp
open ILBuilder

open ReflectionProvider
type R = Reflected<Assemblies = "mscorlib;ILBuilder">

// Create assembly MyAssembly.dll
assembly @"c:\temp\MyAssembly.dll" {
    // Create 2 types "Test_0" and "Test_1"
    for n in [0..1] do
        do! publicType (sprintf "Test_%d" n) {
            // Default empty constructor
            do! publicDefaultEmptyConstructor
        
            // Property "MyProp" with autoimplemented getter and setter
            do! publicAutoProperty<string> "MyProp" { get; set }
            
            // Public method "GetFoo" that would simply return "Bar"
            do! publicMethod<string> "GetFoo" [] {
                ldstr "Bar"
                ret
            }
            
            // Public void method "DoiIt" that would print hash code of String type
            do! publicVoidMethod "DoIt" [] {
                ldtoken (Type typeof<System.String>)
                call R.System.Type.``GetTypeFromHandle : System.RuntimeTypeHandle -> System.Type``
                callvirt R.System.Type.``GetHashCode : unit -> int``
                call R.System.Console.``WriteLine : int -> unit``
                ret
            }
            
            // More properties "Prop_0", "Prop_1", "Prop_2" with auto getter and setter
            for n in [0..2] do
                do! publicAutoProperty<string> (sprintf "Prop_%d" n) { get; set }
        }
} |> saveAssembly
```
