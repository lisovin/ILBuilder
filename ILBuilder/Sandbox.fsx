#r @"IKVM.Reflection.dll"
#r "bin/Debug/ILBuilder.dll"

open ILBuilder
//open Providers

//type Methods = MethodProvider
//let mi = MethodProvider.Methods.System.Console.``WriteLine : string->unit``

let ab = assembly @"c:\temp\MyAssembly.dll" {
    for n in [0..5] do
        do! publicType (sprintf "Test_%d" n) {
            do! publicDefaultEmptyConstructor
        
            do! publicAutoProperty<string> "MyProp" { get; set }
            do! publicMethod<string> "GetFoo" [] {
                ldstr "Bar"
                ret
            }
            
            do! publicVoidMethod "DoIt" [] {
                ldstr "Test out"
                call mi
            }
            
            for n in [0..5] do
                do! publicAutoProperty<string> (sprintf "Prop_%d" n) { get; set }
        }
} 

let ty = ab.GetTypes() |> Seq.head
let u = new IKVM.Reflection.Universe()
u.
System.Activator.CreateInstance(ty.UnderlyingSystemType)

ab |> saveAssembly

Microsoft.FSharp.Core.Prin
typeof<Microsoft.FSharp.Core.Printf+1>
#r @"c:\temp\MyAssembly.dll"
let t0 = Test_0()
t0.MyProp<- "t0"
t0.MyProp


let t1 = Test_1()
//t.
t1.Prop_0 <- "asdf"
t1.Prop_0

t.Prop_1
t.GetFoo() |> printfn "%s"
let s = t.GetFoo()
s
let s = t.VoidMethod()
t.MyProp <- "asdf"
t.MyProp

t.GetType().FullName

let ts = typeof<int>.Assembly.ExportedTypes

