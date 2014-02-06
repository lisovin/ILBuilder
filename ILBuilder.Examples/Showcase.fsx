#r @"C:\Dev\Projects\ILBuilder\ILBuilder\bin\Debug\IKVM.Reflection.dll"
#r @"C:\Dev\Projects\ILBuilder\ILBuilder\bin\Debug\ILBuilder.dll"
open ILBuilder

open System
open System.Reflection
open ReflectionProvider
type R = Reflected<"">

assembly @"c:\temp\MyAssembly.dll" {
    for n in [0..1] do
        do! publicType (sprintf "Test_%d" n) {
            do! publicDefaultEmptyConstructor
        
            do! publicAutoProperty<string> "MyProp" { get; set }
            
            do! publicProperty<string> "JustGet" {
                do! get {
                    ldc_i4_2
                    newarr typeof<char>
                    dup
                    ldc_i4_0
                    ldc_i4_s 'a'
                    stelem typeof<char>
                    dup
                    ldc_i4_1
                    ldc_i4_s 'b'
                    stelem typeof<char>
                    newobj (NewobjArg.Constructor R.System.String.``new : char[] -> string``)
                    ret
                }
            }

            do! publicMethod<string> "GetFoo" [] {
                ldstr "Bar"
                ret
            }

            do! publicVoidMethod "DoIt" [] {
                ldtoken (LdtokenArg.Type typeof<System.String>)
                call R.System.Type.``GetTypeFromHandle : System.RuntimeTypeHandle -> System.Type``
                callvirt R.System.Type.``GetHashCode : unit -> int``
                call R.System.Console.``WriteLine : int -> unit``
                ret
            }
            
            for n in [0..2] do
                do! publicAutoProperty<string> (sprintf "Prop_%d" n) { get; set }
        }
} |> saveAssembly

#r @"c:\temp\MyAssembly.dll"
let t0 = Test_0()

t0.JustGet
t0.DoIt()

t0.MyProp<- "t0"
t0.MyProp


let t1 = Test_1()
t1.Prop_0 <- "asdf"
t1.Prop_0

