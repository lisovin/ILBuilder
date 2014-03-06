namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open ILBuilder
open ILBuilder.Utils

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type FooBar private () = 
    do  let s = "adsf"
        printfn "--->%s" s

    member __.Deleg(s : string) = "string"
    member __.Deleg(i : int) = "int"

    member __.DoIt(o : obj) = 
        if o.GetType() = typeof<string>
        then __.Deleg(o :?> string)
        else if o.GetType() = typeof<int>
        then __.Deleg(o :?> int)
        else "nothing"
        
            
type BuilderType =
| ClrType of System.Type
| GenType of Type
    static member ToGenType (u : Universe) x = 
        match x with
        | ClrType ty -> ty |> Utils.ofType u
        | GenType ty -> ty

type IKVMILBuilder() = 
    let letBind define definesBinding = 
        fun (u : Universe, ilg : ILGenerator) ->
            let bound = define(u, ilg) 
            let defines = definesBinding(bound)
            defines(u, ilg)

    member __.Bind(define, definesBinding) = letBind define definesBinding 

    member __.YieldFrom(x) = fun (u : Universe, ilg : ILGenerator) -> x(u, ilg) |> ignore

    member __.Return(x) = fun (u : Universe, ilg: ILGenerator) -> x

    member __.Zero() = fun (u : Universe, ilg : ILGenerator) -> ()

    member __.Delay(f) = 
        fun (u : Universe, ilg : ILGenerator) -> 
            let g = f()
            g(u, ilg)

    member __.Combine(f1, f2) = __.Bind(f1, fun () -> f2)

    member __.For(xs, f) = 
        xs |> Seq.fold  (fun acc x -> __.Bind(acc, fun () -> f x)) (__.Zero())
        
    member __.Run(f : Universe * ILGenerator -> 'a) = f
        
type IKVMMethodBuilder(name : string, atts, returnType, parameterTypes, ?customAttribute, ?export : string * int) = 
    inherit IKVMILBuilder()

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let methodBuilder = tb.DefineMethod(name, atts)
            methodBuilder.SetReturnType(returnType |> BuilderType.ToGenType u)
            methodBuilder.SetParameters(parameterTypes |> Array.map (BuilderType.ToGenType u))
            match customAttribute with 
            | Some ci -> 
                let cab = CustomAttributeBuilder(ci |> ofConstructorInfo u, [||])
                methodBuilder.SetCustomAttribute(cab)
            | _ -> ()

            let il = methodBuilder.GetILGenerator()
            f (u, il) |> ignore
            //g() |> ignore
            match export with
            |Some (name, n) -> methodBuilder.__AddUnmanagedExport(name, n)
            |_ -> ()
            
            methodBuilder

type IKVMConstructorBuilder(atts, parameterTypes, ?customAttribute) = 
    inherit IKVMILBuilder()

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let args = parameterTypes |> Array.map (BuilderType.ToGenType u)
            let cb = tb.DefineConstructor(atts, CallingConventions.Any, args)

            match customAttribute with 
            | Some ci -> 
                let cab = CustomAttributeBuilder(ci |> ofConstructorInfo u, [||])
                cb.SetCustomAttribute(cab)
            | _ -> ()

            let il = cb.GetILGenerator()
            f (u, il) |> ignore
            cb
            
type PropertyAccessor = Get | Set

type IKVMPropertyAccessorBuilder(accessor) = 
    inherit IKVMILBuilder()

    let prefix = match accessor with | Get -> "get" | Set -> "set"

    member __.Run(f) =
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) ->
            let methodBuilder = tb.DefineMethod(prefix + "_" + pb.Name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, pb.PropertyType, Type.EmptyTypes)
            let il = methodBuilder.GetILGenerator()
            f(u, il)
            match accessor with
            | Get -> pb.SetGetMethod(methodBuilder)
            | Set -> pb.SetSetMethod(methodBuilder)
        
type IKVMAutoPropertyBuilder(name : string, atts, returnType, parameterTypes) = 
    let mutable propField : FieldBuilder = null

    [<CustomOperation("get")>]
    member __.Getter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("get_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, returnType, System.Type.EmptyTypes |> Array.map ClrType)
            let getter = il {
                do! IL.ldarg_0
                do! IL.ldfld propField
                do! IL.ret
            }
            pb.SetGetMethod(getter(u, tb))

    [<CustomOperation("set")>]
    member __.Setter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("set_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, ClrType typeof<System.Void>, [|returnType|])
            let setter = il {
                do! IL.ldarg_0
                do! IL.ldarg_1
                do! IL.stfld propField
                do! IL.ret
            }
            pb.SetSetMethod(setter(u, tb))

    member __.Yield(unit) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> ()

    member __.Zero() = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> ()

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) -> 
            let rt = returnType |> BuilderType.ToGenType u
            let pts = parameterTypes |> Array.map (BuilderType.ToGenType u)
            let pb = tb.DefineProperty(name, PropertyAttributes.None, rt, pts)
            propField <- tb.DefineField(name, rt, FieldAttributes.Private)
            f(u, tb, pb) |> ignore
            pb

type IKVMPropertyBuilder(name : string, atts, returnType, parameterTypes) = 
    member __.Bind(define, definesBinding : unit -> Universe * TypeBuilder * PropertyBuilder -> 'r) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) ->
            let result = define(u, tb, pb)
            let defines = definesBinding()
            defines(u, tb, pb)

    member __.YieldFrom(x) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> x(u, tb, pb) |> ignore

    member __.Return(x) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> x

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) -> 
            let pb = tb.DefineProperty(name, PropertyAttributes.None, returnType |> BuilderType.ToGenType u, parameterTypes |> Utils.ofTypes u)
            f(u, tb, pb) |> ignore
            pb
            
type IKVMTypeBuilder(name, atts, ?customAttribute) = 
    let letBind define definesBinding = 
        fun (u : Universe, tb : TypeBuilder) ->
            let bound = define(u, tb)
            let defines = definesBinding(bound)
            defines(u, tb)

    member __.Bind(define : Universe * TypeBuilder -> 'a, definesBinding) = letBind define definesBinding

    member __.YieldFrom(x) = fun (u : Universe, tb : TypeBuilder) -> x(u, tb) |> ignore

    member __.Return(x) = fun (u : Universe, tb : TypeBuilder) -> x

    member __.Zero() = fun (u : Universe, tb : TypeBuilder) -> ()

    member __.Delay(f) =
        fun (u : Universe, tb : TypeBuilder) ->
            let g = f()
            g (u, tb)

    member __.Combine(f1, f2) = __.Bind(f1, fun () -> f2)

    member __.For(xs, f) = xs |> Seq.fold  (fun acc x -> __.Bind(acc, fun () -> f x)) (__.Zero())

    member __.Run(f) =   
        fun (universe : Universe, moduleBuilder : ModuleBuilder) ->
            let typeBuilder = moduleBuilder.DefineType(name, atts)
            match customAttribute with
            | Some ci ->
                let cab = CustomAttributeBuilder(ci |> ofConstructorInfo universe, [||])
                typeBuilder.SetCustomAttribute(cab)
            | _ -> ()
            f(universe, typeBuilder) |> ignore
            typeBuilder.CreateType() 

type IKVMNestedTypeBuilder(name, atts) = 
    inherit IKVMTypeBuilder(name, atts)

    member __.Run(f) =   
        fun (u : Universe, typeBuilder : TypeBuilder) -> 
            let tb = typeBuilder.DefineNestedType(name, atts)
            f(u, tb) |> ignore
            tb.CreateType()

type IKVMAssemblyBuilder() = 
    member __.Bind(define, definesBinding) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            let bound = define(u, mb)
            let rest = definesBinding(bound)
            rest(u, mb)
    (*
    member __.Bind(define, definesBinding) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            let bound = define(u, mb)
            //definesBinding()
            let rest : Universe * ModuleBuilder -> unit = definesBinding()
            rest(u, mb) 
      *) 
    member __.Return(x) = fun (u : Universe, mb : ModuleBuilder) -> x

    member __.Zero() = fun (u : Universe, mb : ModuleBuilder) -> ()

    member __.YieldFrom(x) = fun (u : Universe, mb : ModuleBuilder) -> x(u, mb) |> ignore

    member __.Delay(f) =
        fun (u : Universe, mb : ModuleBuilder) ->
            let g = f()
            g (u, mb)

    member __.Combine(f1, f2) = __.Bind(f1, fun () -> f2)

    member __.For(xs, f) = xs |> Seq.fold  (fun acc x -> __.Bind(acc, fun () -> f x)) (__.Zero())
    (*
    member __.For(xs, f) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            for x in xs do
                let d = f(x)
                d(u, mb)
            //define() |> ignore
            *)
    member __.Run(f) =   
        fun (universe : Universe, assemblyPath) ->
            let name = Path.GetFileNameWithoutExtension(assemblyPath)
            let filename = Path.GetFileName(assemblyPath)
            let directory = Path.GetDirectoryName(assemblyPath)
            let assemblyName = AssemblyName(name)
            let assemblyBuilder = universe.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save, directory)
            let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, filename)
            f(universe, moduleBuilder) |> ignore
            assemblyBuilder

[<AutoOpen>]
module Helpers =
    let assembly = IKVMAssemblyBuilder()

    let il = IKVMILBuilder()


    let propertyAccessor kind = IKVMPropertyAccessorBuilder(kind)
    let get = propertyAccessor Get
    let set = propertyAccessor Set
(*
    let declareLocal<'TValue> = 
        fun (u : Universe, il : ILGenerator) -> 
            il.DeclareLocal(ofType u typeof<'TValue>)
            *)
    (*
     * Save
     *)
    let saveAssembly assemblyPath (f : Universe * string -> AssemblyBuilder) = 
        use universe = new Universe()
        universe.add_AssemblyResolve(fun sender args ->
            let path = AppDomain.CurrentDomain.Load(args.Name).Location
            if File.Exists(path)
            then universe.LoadFile(path)
            else null)

        let ab = f (universe, assemblyPath)
        let name = ab.GetName().Name + ".dll"
        ab.Save(name)
