//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"
#load "ILExtensions.fs"

open System
open ILBuilder

let tables = ["FooBar2"]
let toTableName name = name

(*
<@
publicType "Foo" {
    let! cons = fun (u,tb) -> Unchecked.defaultof<IKVM.Reflection.Emit.ConstructorBuilder> 
    do! (fun (u, tb) -> "")
}
@>
*)

assembly {
    for t in tables do
        let tableName = toTableName t
        
        let! ty = IL.publicType tableName {
            //printfn "doing Table_%d" n
            //yield! IL.publicDefaultConstructor

            yield! IL.publicConstructor {
                printfn "--->constructor foobar"
                do! IL.ldarg_0
                do! IL.callvirt (typeof<obj>.GetConstructor([||]))
                do! IL.ret
            }

            yield! IL.nestedPublicType "Foo" {
                yield! IL.publicDefaultConstructor
                for i in 0..2 do
                    yield! IL.publicAutoProperty<string> "Bar" { get; }
                    printfn "-->bar"
            }
        }
        printfn "%A" ty
        //let! ty2 = fun (u, ilg) -> typeof<obj>
        yield! IL.publicType ("Foo.Table2") {
            printfn "doing foo.table2"
            let! cons = IL.publicDefaultConstructor
            
            yield! IL.publicStaticMethod<string>("Test") {
                do! IL.ldstr ""
                do! IL.ret
            }

            //printfn "%A" m.Name
            for n in 0..3 do
                yield! IL.publicAutoProperty<string> (sprintf "Foo_%d" n) { get; }
                //do! IL.call m

            let! thisType = IL.thisType
            yield! IL.publicStaticMethod(thisType, "Query", typeof<string>) {
                do! IL.newobj(cons : IKVM.Reflection.Emit.ConstructorBuilder)
                do! IL.ldstr "Foo"
                do! IL.ret
            } 
            
            yield! IL.publicStaticMethod<string>("Insert", typeof<string>) {
                do! IL.newobj cons
                do! IL.ldstr "Foobar"
                do! IL.ret
            }
        }
        
        let! ty2 = IL.publicType ("Db." + tableName) {
            let columns = ""
            let values = ""
            
            yield! IL.nestedPublicType tableName {
                yield! IL.publicStaticMethod<string>("Insert", ty) {
                    do! IL.ldstr "adsf"
                    do! IL.ret
                }
            }
            
            printfn "end"
        } 

        do! (fun (_) -> printfn "%A" ty)
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
FooBar2()

Foo.Table2.Test()
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let f() = ""
let g (s : string) = 1

<@ f @>
<@ g @>
match <@ g @> with 
| Lambda(_, Call(_, _, [])) -> printfn "doBind"
| _ -> printfn "letBind"

