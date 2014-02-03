#r @"C:\Dev\Projects\ILBuilder\ILBuilder\bin\Debug\IKVM.Reflection.dll"
#r @"C:\Dev\Projects\ILBuilder\ILBuilder\bin\Debug\ILBuilder.dll"

open ILBuilder

open ReflectionProvider
type R = Reflected<Assemblies = "mscorlib;ILBuilder">


assembly @"c:\temp\MyAssembly.dll" {
    for n in [0..1] do
        do! publicType (sprintf "Test_%d" n) {
            do! publicDefaultEmptyConstructor
        
            do! publicAutoProperty<string> "MyProp" { get; set }
            do! publicMethod<string> "GetFoo" [] {
                ldstr "Bar"
                ret
            }
            
            do! publicVoidMethod "DoIt" [] {
                ldtoken (Type typeof<System.String>)
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
t0.DoIt()

t0.MyProp<- "t0"
t0.MyProp


let t1 = Test_1()
t1.Prop_0 <- "asdf"
t1.Prop_0

