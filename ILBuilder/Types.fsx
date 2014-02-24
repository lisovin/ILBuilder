//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "IL.fs"
#load "Builders.fs"

open System
open ILBuilder

let tables = ["FooBar"]
let toTableName name = name

assembly {
    for t in tables do
        let tableName = toTableName t
        
        let! ty = publicType tableName {
            //printfn "doing Table_%d" n
            do! publicDefaultEmptyConstructor
        }
        printfn "%A" ty
        //let! ty2 = fun (u, ilg) -> typeof<obj>
        do! publicType ("Foo.Table2") {
            printfn "doing foo.table2"
            let! cons = publicDefaultEmptyConstructor
            let! m = publicStaticMethod<string> "Test" [] {
                do! IL.ldstr ""
                do! IL.ret
            }

            for n in 0..3 do
                do! publicAutoProperty<string> (sprintf "Foo_%d" n) { get; }
                
            //let! self = IL.thisType
            let cb = cons
            do! publicStaticMethodOfType ThisType "Query" [ClrType typeof<string>] {
                do! IL.newobj cb
                do! IL.ldstr "Foo"
                do! IL.ret
            }
            
            do! publicStaticMethod<string> "Insert" [ClrType typeof<string>] {
                do! IL.ldstr "Foobar"
                do! IL.ret
            }
        }
        
        let! ty2 = publicType ("Db." + tableName) {
            let columns = ""
            let values = ""
            
            do! (nestedPublicType tableName {
                do! publicStaticMethod<string> "Insert" [GenType ty] {
                    do! IL.ldstr "adsf"
                    do! IL.ret
                }
            } >> ignore)
            
            printfn "end"
        } 

        printfn "%A" ty 
} |> saveAssembly @"c:\temp\test.dll"



