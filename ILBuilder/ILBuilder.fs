module ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open Microsoft.FSharp.Quotations

//type State<'s> = State of ('s -> unit)
type IKVMMethodBuilder(name : string, atts, returnType, parameterTypes, ?export : string * int) = 
    inherit IKVM.EmitBuilder()

    member __.Run(f : Universe * ILGenerator -> unit) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let methodBuilder = tb.DefineMethod(name, atts, returnType |> IKVM.ofType u, parameterTypes |> IKVM.ofTypes u)
            let il = methodBuilder.GetILGenerator()
            f (u, il)
            match export with
            |Some (name, n) -> methodBuilder.__AddUnmanagedExport(name, n)
            |_ -> ()
            methodBuilder

type IKVMAutoPropertyBuilder(name : string, atts, returnType, parameterTypes) = 
    let mutable propField = null

    [<CustomOperation("get")>]
    member __.Getter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("get_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, returnType, System.Type.EmptyTypes)
            let getter = il {
                ldarg_0
                ldfld propField
                ret
            }
            pb.SetGetMethod(getter(u, tb))

    [<CustomOperation("set")>]
    member __.Setter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("set_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, null, [|returnType|])
            let setter = il {
                ldarg_0
                ldarg_1
                stfld__FieldBuilder propField
                ret
            }
            pb.SetSetMethod(setter(u, tb))

    member __.Yield(unit) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> ()

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) -> 
            let pb = tb.DefineProperty(name, PropertyAttributes.None, returnType |> IKVM.ofType u, parameterTypes |> IKVM.ofTypes u)
            propField <- tb.DefineField(name, returnType |> IKVM.ofType u, FieldAttributes.Private)
            f(u, tb, pb) |> ignore
            pb
            
type IKVMTypeBuilder(name, atts) = 
    // let!
    member __.Bind(define, definesBinding : 'b -> Universe * TypeBuilder -> 'r) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let bound = define(u, tb)
            let defines = definesBinding(bound)
            defines(u, tb)
    // do!
    member __.Bind(define, definesBinding : unit -> Universe * TypeBuilder -> 'r) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let result = define(u, tb)
            let defines = definesBinding()
            defines(u, tb)
    
    member __.Return(x) = fun (u : Universe, tb : TypeBuilder) -> x

    member __.For(xs, f) = 
        fun (u : Universe, tb : TypeBuilder) ->
            for x in xs do
                let define = f(x)
                define(u, tb)

    member __.Run(f) =   
        fun (universe : Universe, moduleBuilder : ModuleBuilder) ->
            let typeBuilder = moduleBuilder.DefineType(name, atts)
            f(universe, typeBuilder) |> ignore
            typeBuilder.CreateType() 

type IKVMAssemblyBuilder(assemblyPath) = 
    let universe = new Universe()
    let name = Path.GetFileNameWithoutExtension(assemblyPath)
    let filename = Path.GetFileName(assemblyPath)
    let directory = Path.GetDirectoryName(assemblyPath)
    let assemblyName = AssemblyName(name)
    let universe = new Universe()
    let assemblyBuilder = universe.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save, directory)
    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, filename)

    member __.Bind(define, definesBinding : 'b -> 'r) = 
        let bound = define(universe, moduleBuilder)
        definesBinding(bound)

    member __.Bind(define, definesBinding : unit -> 'r) = 
        let result = define(universe, moduleBuilder)
        definesBinding()

    member __.Return(x) = x

    //member __.Zero() = () 

    member __.For(xs, f) = 
        for x in xs do
            f(x)
            //define() |> ignore

    member __.Run(f) =   
        assemblyBuilder

let assembly assemblyPath = IKVMAssemblyBuilder(assemblyPath)

(*
 * Types
 *)
let publicType typeName = 
    IKVMTypeBuilder(typeName, TypeAttributes.Public)

(*
 * Constructors
 *)
let publicConstructor (u : Universe, mb : ModuleBuilder) = ()

let publicDefaultEmptyConstructor (u : Universe, tb : TypeBuilder) =
    tb.DefineDefaultConstructor(MethodAttributes.Public)

(*
 * Methods
 *)
let publicMethod<'TReturnType> name (parameterTypes : seq<System.Type>) = 
    let returnType = 
        match typeof<'TReturnType> with 
        | t when t = typeof<unit> -> typeof<System.Void>
        | t -> t
    IKVMMethodBuilder(name, MethodAttributes.Public, returnType, parameterTypes |> Seq.toArray)

let publicVoidMethod name parameterTypes = 
    publicMethod<unit> name parameterTypes
    
let privateStaticMethod methodName returnType parameters = 
    IKVMMethodBuilder(methodName, MethodAttributes.Private ||| MethodAttributes.Static, returnType, parameters)

let publicStaticMethod_Exported methodName returnType parameters exportIndex = 
    IKVMMethodBuilder(methodName, MethodAttributes.Public ||| MethodAttributes.Static, returnType, parameters, (methodName, exportIndex))
    
(*
 * Fields
 *)
let privateStaticField fieldName fieldType =
    fun (tb : TypeBuilder, u : Universe) ->
        tb.DefineField(fieldName, u.Import(fieldType), FieldAttributes.Static ||| FieldAttributes.Private)         

let publicStaticField fieldName fieldType =
    fun (tb : TypeBuilder, u : Universe) ->
        tb.DefineField(fieldName, u.Import(fieldType), FieldAttributes.Static ||| FieldAttributes.Public)         
    
let publicField fieldName fieldType =
    fun (tb : TypeBuilder, u : Universe) ->
        tb.DefineField(fieldName, u.Import(fieldType), FieldAttributes.Public)         
        
(*
 * Properties
 *)
let publicAutoProperty<'t> propName = 
    IKVMAutoPropertyBuilder(propName, PropertyAttributes.None, typeof<'t>, [||])

(*
 * Save
 *)
let saveAssembly (ab : AssemblyBuilder) = 
    ab.Save(ab.GetName().Name + ".dll")