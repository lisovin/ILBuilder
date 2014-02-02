#r @"..\ILBuilder\bin\Debug\IKVM.Reflection.dll"
#r @"..\ILBuilder\bin\Debug\ILBuilder.dll"

open ILBuilder
open MethodsProvider

type SystemMethods = Methods<Assembly = "mscorlib">
type ILBuilderMethods = Methods<Assembly = "ILBuilder">

typeof<ILBuilder.IKVMTypeBuilder>.GetConstructors().[0]

assembly @"c:\temp\MyAssembly.dll" {
    for n in [0..0] do
        do! publicType (sprintf "Test_%d" n) {
            do! publicDefaultEmptyConstructor
        
            do! publicAutoProperty<string> "MyProp" { get; set }
            do! publicMethod<string> "GetFoo" [] {
                ldstr "Bar"
                ret
            }
            
            do! publicVoidMethod "DoIt" [] {
                ldtoken (IKVM.Type typeof<System.String>)
                call SystemMethods.System.Type.``GetTypeFromHandle : System.RuntimeTypeHandle -> System.Type``
                callvirt SystemMethods.System.Type.``GetHashCode : unit -> int``
                call SystemMethods.System.Console.``WriteLine : int -> unit``
                ret
            }
            
            for n in [0..0] do
                do! publicAutoProperty<string> (sprintf "Prop_%d" n) { get; set }
        }
} |> saveAssembly

#r @"c:\temp\MyAssembly.dll"
let t0 = Test_0()
t0.DoIt()

t0.MyProp<- "t0"
t0.MyProp

t0.GetFoo()