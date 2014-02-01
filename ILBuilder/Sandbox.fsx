#r @"IKVM.Reflection.dll"
#r "bin/Debug/ILBuilder.dll"

open ILBuilder

assembly @"c:\temp\MyAssembly.dll" {
    do! publicType "Test" {
        do! publicDefaultEmptyConstructor
        do! publicAutoProperty<string> "MyProp" { get; set }
        do! publicVoidMethod "VoidMethod" [] {
            ldstr "asdf"
        }
    }
} |> saveAssembly


#r @"c:\temp\MyAssembly.dll"
let t = Test()
let s = t.VoidMethod()
t.MyProp <- "asdf"
t.MyProp

t.GetType().FullName
