//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

let props = ["Foo"; "Bar"]

assembly {
    do! publicType ("Test") {
        let! cons = publicDefaultEmptyConstructor
        printfn "-->const: %A" cons
        (*
        let fields = ref []
        let! prop = publicAutoProperty<string> "XYZ" { set; get; }
        //printfn "--->a prop: %A" prop
        let! prop = publicAutoProperty<int> "Foo" { set; get; }
        printfn "--->prop: %A" prop
        fields := prop :: !fields

        let! prop = publicAutoProperty<string> "Bar" { set; get; }
        printfn "--->prop: %A" prop
        fields := prop :: !fields
        *)
        (*
        printfn "--->fields: %A" fields
        let n = 5
        yield! publicMethod<obj[]> "DoIt" [] {
            printfn "--->generate DoIt"
            //do! emitArray props
            yield! IL.emitArrayOfValues<obj>(!fields)

            //do! IL.call (typeof<Console>.GetMethod("WriteLine", [| typeof<obj> |]))
            yield! IL.ret
        } 
        *)
    }
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
let t = Test(Foo = 1, Bar = "adsf")
t.DoIt()
