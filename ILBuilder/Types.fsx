//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

let tables = ["FooBar"]
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
        
        let! ty = publicType tableName {
            //printfn "doing Table_%d" n
            yield! publicDefaultEmptyConstructor
        }
        printfn "%A" ty
        //let! ty2 = fun (u, ilg) -> typeof<obj>
        do! publicType ("Foo.Table2") {
            printfn "doing foo.table2"
            let! cons = publicDefaultEmptyConstructor
            
            yield! publicStaticMethod<string> "Test" [] {
                do! IL.ldstr ""
                do! IL.ret
            }

            //printfn "%A" m.Name
            for n in 0..3 do
                yield! publicAutoProperty<string> (sprintf "Foo_%d" n) { get; }
                //do! IL.call m

            yield! publicStaticMethodOfType ThisType "Query" [ClrType typeof<string>] {
                do! IL.newobj(cons : IKVM.Reflection.Emit.ConstructorBuilder)
                do! IL.ldstr "Foo"
                do! IL.ret
            } 
            
            let! (self : IKVM.Reflection.Type) = declaringType
            yield! publicStaticMethod<string> "Insert" [ClrType typeof<string>] {
                do! IL.newobj self
                do! IL.ldstr "Foobar"
                do! IL.ret
            }
        }
        
        let! ty2 = publicType ("Db." + tableName) {
            let columns = ""
            let values = ""
            
            yield! nestedPublicType tableName {
                do! publicStaticMethod<string> "Insert" [GenType ty] {
                    do! IL.ldstr "adsf"
                    do! IL.ret
                }
            }
            
            printfn "end"
        } 

        do! (fun (_) -> printfn "%A" ty)
} |> saveAssembly @"c:\temp\test.dll"

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

let f() = ""
let g (s : string) = 1

<@ f @>
<@ g @>
match <@ g @> with 
| Lambda(_, Call(_, _, [])) -> printfn "doBind"
| _ -> printfn "letBind"

