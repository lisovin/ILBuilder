//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
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

        do! publicType ("Foo.Table2") {
            printfn "doing foo.table2"
            let! cons = publicDefaultEmptyConstructor
            let! m = publicStaticMethod<string> "Test" [] {
                ret
            }

            for n in 0..3 do
                do! publicAutoProperty<string> (sprintf "Foo_%d" n) { get; }
                
            do! publicStaticMethodOfType ThisType "Query" [ClrType typeof<string>] {
                newobj (IkvmConstructor cons)
                ret
            }

            do! publicStaticMethod<string> "Insert" [ ThisType ] {
                ldstr "Foobar"
                ret
            }
        }
        
        let! ty2 = publicType ("Db." + tableName) {
            let columns = ""
            let values = ""
            
            do! nestedPublicType tableName {
                do! publicStaticMethod<string> "Insert" [IkvmType ty] {
                    ldstr "adsf"
                    ret
                }
            }
            
            printfn "end"
        } 
        printfn "%A" ty2
} |> saveAssembly @"c:\temp\test.dll"

#r @"c:\temp\test.dll"
Foo.Table2.Query("")
Foo.Table2.Insert(Foo.Table2())

Db.FooBar.FooBar.Insert(FooBar())

