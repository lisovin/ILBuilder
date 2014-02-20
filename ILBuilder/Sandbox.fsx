//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
#load "Utils.fs"
#load "Builders.fs"

open System
open ILBuilder

let ns = [0..3]
let asm = assembly {
    (*
    let! ty = publicType "Table" {
        do! publicDefaultEmptyConstructor 
    } 
    printfn "%A" ty

    do! publicType ("Foo.Table") {
        printfn "doing foo.table"
        do! publicDefaultEmptyConstructor
    }
    
    do! publicType ("Foo.Table2") {
        printfn "doing foo.table2"
        do! publicDefaultEmptyConstructor
    }
    *)
    for n in ns do
        let name = sprintf "Table_%d" n
        let! ty = publicType name {
            //printfn "doing Table_%d" n
            do! publicDefaultEmptyConstructor
        }

        do! publicType ("Foo.Table2") {
            printfn "doing foo.table2"
            do! publicDefaultEmptyConstructor
        }
} 

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
            do! publicDefaultEmptyConstructor
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




