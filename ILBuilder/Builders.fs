namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open ILBuilder
open ILBuilder.Utils

type BuilderType =
| ThisType
| ClrType of System.Type
| GenType of Type
    static member ToGenType (u : Universe) (declaringType : Type) x = 
        match x with
        | ThisType -> declaringType
        | ClrType ty -> ty |> Utils.ofType u
        | GenType ty -> ty

type IKVMILBuilder() = 
    let letBind define definesBinding = 
        fun (u : Universe, ilg : ILGenerator) ->
            let bound = define(u, ilg) 
            let defines = definesBinding(bound)
            defines(u, ilg)

    let doBind define definesBinding = 
        fun (u : Universe, ilg : ILGenerator) ->
            let result = define(u, ilg)
            let defines = definesBinding()
            defines(u, ilg)

    // let!
    member __.Bind(define, definesBinding) = letBind define definesBinding 

    // do!
    member __.Bind(define : Universe * ILGenerator -> #MemberInfo, definesBinding) = doBind define definesBinding

    member __.Return(x) = fun (u : Universe, ilg: ILGenerator) -> x

    member __.Zero() = ()//fun (u : Universe, ilg : ILGenerator) -> ()

    member __.Delay(f) = f()

    member __.Combine(f1, f2) =
        fun (u : Universe, ilg : ILGenerator) ->
            f1(u, ilg)    
            f2(u, ilg)

    member __.For(xs, f) = 
        fun (u : Universe, ilg : ILGenerator) ->
            for x in xs do
                let emit = f(x)
                emit(u, ilg)

    member __.Run(f : Universe * ILGenerator -> unit) = 
        fun (u : Universe, il : ILGenerator) ->
            f (u, il)
        
type IKVMMethodBuilder(name : string, atts, returnType, parameterTypes, ?export : string * int) = 
    inherit IKVMILBuilder()

    member __.Run(f : Universe * ILGenerator -> unit) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let methodBuilder = tb.DefineMethod(name, atts)
            methodBuilder.SetReturnType(returnType |> BuilderType.ToGenType u tb.DeclaringType)
            methodBuilder.SetParameters(parameterTypes |> Array.map (BuilderType.ToGenType u tb.DeclaringType))
            let il = methodBuilder.GetILGenerator()
            f (u, il)
            match export with
            |Some (name, n) -> methodBuilder.__AddUnmanagedExport(name, n)
            |_ -> ()
            methodBuilder

type PropertyAccessor = Get | Set

type IKVMPropertyAccessorBuilder(accessor) = 
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
            let rt = returnType |> BuilderType.ToGenType u tb.DeclaringType
            let pts = parameterTypes |> Array.map (BuilderType.ToGenType u tb.DeclaringType)
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

    member __.Return(x) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> x

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) -> 
            let pb = tb.DefineProperty(name, PropertyAttributes.None, returnType |> Utils.ofType u, parameterTypes |> Utils.ofTypes u)
            f(u, tb, pb) |> ignore
            pb
            
type GenTypeBuilder(name, atts) = 
    let letBind define definesBinding = 
        fun (u : Universe, tb : TypeBuilder) ->
            let bound = define(u, tb) 
            let defines :  Universe * TypeBuilder -> unit = definesBinding(bound)
            defines(u, tb)

    let doBind define definesBinding = 
        fun (u : Universe, tb : TypeBuilder) ->
            let result = define(u, tb)
            let defines :  Universe * TypeBuilder -> unit = definesBinding()
            defines(u, tb)
        
    // let!
    member __.Bind(define : Universe * TypeBuilder -> ConstructorBuilder, definesBinding) = letBind define definesBinding
    member __.Bind(define : Universe * TypeBuilder -> IKVMMethodBuilder, definesBinding) = letBind define definesBinding 

    // do!
    member __.Bind(define, definesBinding) = doBind define definesBinding
    
    member __.Return(x) = fun (u : Universe, tb : TypeBuilder) -> x

    member __.Zero() = fun (u : Universe, tb : TypeBuilder) -> ()

    member __.Delay(f) = f()

    member __.Combine(f1, f2) =
        fun (u : Universe, tb : TypeBuilder) ->
            f1(u, tb)    
            f2(u, tb)

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

type IKVMNestedTypeBuilder(name, atts) = 
    // let!
    member __.Bind(define, definesBinding) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let bound : ConstructorBuilder = define(u, tb)
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
        fun (u : Universe, typeBuilder : TypeBuilder) -> 
            let tb = typeBuilder.DefineNestedType(name, atts)
            f(u, tb) |> ignore
            tb.CreateType()

//type AR = AR of Universe * ModuleBuilder * (Universe * ModuleBuilder -> 'a)

type IKVMAssemblyBuilder() = 
    member __.Bind(define, definesBinding) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            let bound : IKVM.Reflection.Type = define(u, mb)
            //definesBinding(bound)
            let rest : Universe * ModuleBuilder -> unit = definesBinding(bound)
            rest(u, mb)
    
    member __.Bind(define, definesBinding) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            let bound = define(u, mb)
            //definesBinding()
            let rest : Universe * ModuleBuilder -> unit = definesBinding()
            rest(u, mb) 
       
    member __.Return(x) = fun (u : Universe, mb : ModuleBuilder) -> x

    member __.Zero() = fun (u : Universe, mb : ModuleBuilder) -> ()

    member __.For(xs, f) = 
        fun (u : Universe, mb : ModuleBuilder) ->
            for x in xs do
                let d = f(x)
                d(u, mb)
            //define() |> ignore

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

    (*
     * Types
     *)
    let publicType typeName = GenTypeBuilder(typeName, TypeAttributes.Public)

    let internalType typeName = GenTypeBuilder(typeName, TypeAttributes.NotPublic)

    let nestedPublicType typeName = IKVMNestedTypeBuilder(typeName, TypeAttributes.NestedPublic)

    (*
     * Constructors
     *)
    let publicConstructor (u : Universe, mb : ModuleBuilder) = ()

    let publicDefaultEmptyConstructor (u : Universe, tb : TypeBuilder) =
        tb.DefineDefaultConstructor(MethodAttributes.Public)

    (*
     * Methods
     *)
    let methodOfTypeWithAtts atts returnType name parameterTypes = 
        let returnType = 
            match returnType with 
            | ClrType ty when ty = typeof<unit> -> ClrType typeof<System.Void>
            | t -> t
        IKVMMethodBuilder(name, atts, returnType, parameterTypes |> Seq.toArray)
        
    let publicMethodOfType returnType name parameterTypes = 
        methodOfTypeWithAtts MethodAttributes.Public returnType name parameterTypes

    let publicMethod<'TReturnType> name parameterTypes = 
        (*let returnType = 
            match typeof<'TReturnType> with 
            | t when t = typeof<unit> -> typeof<System.Void>
            | t -> t *)
        let returnType = typeof<'TReturnType>
        methodOfTypeWithAtts MethodAttributes.Public (ClrType returnType) name parameterTypes

    let publicVoidMethod name parameterTypes = 
        publicMethod<unit> name parameterTypes
    
    let privateStaticMethodOfType returnType name parameterTypes = 
       methodOfTypeWithAtts (MethodAttributes.Private ||| MethodAttributes.Static) returnType name parameterTypes

    let privateStaticMethod<'TReturnType> returnType name parameterTypes = 
        privateStaticMethodOfType (ClrType typeof<'TReturnType>) name parameterTypes

    let privateStaticVoidMethod returnType name parameterTypes = 
        privateStaticMethod<unit> name parameterTypes

    let publicStaticMethodOfType returnType name parameterTypes = 
        methodOfTypeWithAtts (MethodAttributes.Public ||| MethodAttributes.Static) returnType name parameterTypes

    let publicStaticMethod<'TReturnType> name parameterTypes = 
        publicStaticMethodOfType (ClrType typeof<'TReturnType>) name parameterTypes

    let publicStaticVoidMethod name parameterTypes = 
        publicStaticMethod<unit> name parameterTypes

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
    let publicAutoPropertyOfType propType propName = 
        IKVMAutoPropertyBuilder(propName, PropertyAttributes.None, propType, [||])

    let publicAutoProperty<'t> propName = 
        publicAutoPropertyOfType (ClrType typeof<'t>) propName

    let publicPropertyOfType propType propName = 
        IKVMPropertyBuilder(propName, PropertyAttributes.None, propType, [||])

    let publicProperty<'t> propName =
        publicPropertyOfType typeof<'t> propName

    let propertyAccessor kind = IKVMPropertyAccessorBuilder(kind)
    let get = propertyAccessor Get
    let set = propertyAccessor Set

    let declareLocal<'TValue> = 
        fun (u : Universe, il : ILGenerator) -> 
            il.DeclareLocal(ofType u typeof<'TValue>)

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