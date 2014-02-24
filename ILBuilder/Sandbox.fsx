//#r "bin/Debug/ILBuilder.dll"
#r "bin/Debug/IKVM.Reflection.dll"
//#load "Utils.fs"
//#load "Builders.fs"
open System
//open ILBuilder

type TestBuilder() = 
    [<CustomOperation("foo", MaintainsVariableSpaceUsingBind = false, AllowIntoPattern = true)>]
    member __.foo (f, num : int) = 
        sprintf "foo %d" num

    [<CustomOperation("bar", MaintainsVariableSpaceUsingBind = true)>]
    member __.bar (s, num : int) =
        sprintf "%s\nbar %d" s num

    member __.Bind(f, r) = 
        let bound = f()
        r(bound)

    member __.Zero() = ""
    member __.Return(x) = "ret: "
    member __.Yield(x) = ""
    member __.Quote() = ()
    member __.Run(f) = f 

let test = TestBuilder()

let t = test {
    foo 5 into g
    //let! x = fun () -> 3
    //printfn "%d" x
    foo x
    //bar x
}

