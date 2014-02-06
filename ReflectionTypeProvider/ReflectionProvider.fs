namespace ReflectionTypeProvider

open System
open System.Collections.Generic
open System.Reflection
open System.IO

open Microsoft.FSharp.Core.CompilerServices

type private Member =
| Method of MethodInfo
| Constructor of ConstructorInfo
    member x.MetadataToken 
        with get() = 
            match x with
            | Method mi -> mi.MetadataToken
            | Constructor c -> c.MetadataToken
    member x.MemberType
        with get() = 
            match x with 
            | Method _ -> typeof<MethodInfo>
            | Constructor _ -> typeof<ConstructorInfo>
    member x.DeclaringType
        with get() = 
            match x with
            | Method mi -> mi.DeclaringType
            | Constructor ci -> ci.DeclaringType            

[<TypeProvider>]
type ReflectionProvider() =
    inherit TypeProviderForNamespaces()

    let staticParams = 
        [
            ProvidedStaticParameter("Assemblies", typeof<string>, "mscorlib")
        ]
    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "ReflectionProvider"
    let containerType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Reflected", None, IsErased = true)

    let lookup = Dictionary()

    let prettyPrintSig mem verbose = 
        let prettyPrintTy = function
        | ty when ty = typeof<bool> -> "bool"
        | ty when ty = typeof<byte> -> "byte"
        | ty when ty = typeof<sbyte> -> "sbyte"
        | ty when ty = typeof<int16> -> "int16"
        | ty when ty = typeof<uint16> -> "uint16"
        | ty when ty = typeof<int> -> "int"
        | ty when ty = typeof<uint32> -> "uint32"
        | ty when ty = typeof<int64> -> "int64"
        | ty when ty = typeof<uint64> -> "uint64"
        | ty when ty = typeof<nativeint> -> "nativeint"
        | ty when ty = typeof<unativeint> -> "unativeint"
        | ty when ty = typeof<char> -> "char"
        | ty when ty = typeof<string> -> "string"
        | ty when ty = typeof<decimal> -> "decimal"
        | ty when ty = typeof<unit> -> "unit"
        | ty when ty = typeof<float32> -> "float32"
        | ty when ty = typeof<float> -> "float"
        | ty when ty = typeof<obj> -> "obj"
        | ty when ty = typeof<System.Void> -> "unit"
        | ty when ty = typeof<bool[]> -> "bool[]"
        | ty when ty = typeof<byte[]> -> "byte[]"
        | ty when ty = typeof<sbyte[]> -> "sbyte[]"
        | ty when ty = typeof<int16[]> -> "int16[]"
        | ty when ty = typeof<uint16[]> -> "uint16[]"
        | ty when ty = typeof<int[]> -> "int[]"
        | ty when ty = typeof<uint32[]> -> "uint32[]"
        | ty when ty = typeof<int64[]> -> "int64[]"
        | ty when ty = typeof<uint64[]> -> "uint64[]"
        | ty when ty = typeof<nativeint[]> -> "nativeint[]"
        | ty when ty = typeof<unativeint[]> -> "unativeint[]"
        | ty when ty = typeof<char[]> -> "char[]"
        | ty when ty = typeof<string[]> -> "string[]"
        | ty when ty = typeof<decimal[]> -> "decimal[]"
        | ty when ty = typeof<unit[]> -> "unit[]"
        | ty when ty = typeof<float32[]> -> "float32[]"
        | ty when ty = typeof<float[]> -> "float[]"
        | ty when ty = typeof<obj[]> -> "obj[]"
        | ty -> ty.FullName
        
        let prettyPrintParm (p:ParameterInfo) =
            if verbose then
                sprintf " %s:%s " p.Name (prettyPrintTy p.ParameterType)
            else
                prettyPrintTy p.ParameterType

        let ps, name, returnType = match mem with
                                   | Method mi -> mi.GetParameters() , mi.Name, mi.ReturnType
                                   | Constructor ci -> ci.GetParameters(), "new", ci.DeclaringType

        sprintf "%s : %s -> %s" name (if ps.Length > 0 then System.String.Join("*", ps |> Array.map prettyPrintParm) else "unit") (prettyPrintTy returnType)

    let rec addNested (t : ProvidedTypeDefinition) parts (mem : Member) level = 
        match parts with
        | [] -> 
            let tok = mem.MetadataToken
            let name = prettyPrintSig mem false
            let ty = mem.DeclaringType 
            let typeName = ty.FullName
            let assemblyName = ty.Assembly.GetName().Name
            t.AddMemberDelayed (fun () -> 
                let p = ProvidedProperty(name, 
                                         mem.MemberType,
                                         IsStatic = true, 
                                         GetterCode = fun args -> 
                                         <@@ //printfn "--->ty: %s" typeName
                                             let asm = Assembly.Load(assemblyName)
                                             let ty = asm.GetType(typeName)
                                             //printfn "--->type: %s" ty.FullName
                                             asm.ManifestModule.ResolveMethod(tok) @@>)
                p.AddXmlDoc(prettyPrintSig mem true)
                p)
        | p::parts ->
            t.AddMembersDelayed(fun () -> 
                let level = p :: level
                match lookup.TryGetValue(level) with
                | true, t -> 
                    addNested t parts mem level
                    []
                | _ -> 
                    //printfn "Add level %s" p
                    let pt = ProvidedTypeDefinition(p, None)
                    lookup.Add(level, pt)
                    addNested pt parts mem level
                    [pt])


    let loader (typeName, assemblyNames : string) = 
        let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, None, IsErased = true)
        let assemblyNames = assemblyNames.Split(';')
                            |> Seq.filter ((<>)String.Empty)
                            |> Seq.append ["mscorlib"]
                            |> Seq.distinct
                            |> Seq.toArray

        for assemblyName in assemblyNames do
            let assembly = Assembly.Load(assemblyName)
            let typesAndMembers = 
                assembly.ExportedTypes
                |> Seq.filter (fun ty -> ty <> typeof<System.Void>)
                |> Seq.collect (fun ty -> 
                    let ms = ty.GetMethods() |> Seq.map (Method)
                    let cs = ty.GetConstructors() |> Seq.map (Constructor)
                    ms |> Seq.append cs
                       |> Seq.map (fun m -> ty, m))
                |> Seq.toArray

            for ty, m in typesAndMembers do
                //printfn "Processing type %s method %A" ty.FullName m
                let parts = (if ty.Namespace = null then [] else ty.Namespace.Split '.' |> Seq.toList) @ [ty.Name]
                addNested t parts m []
        t

    do AppDomain.CurrentDomain.add_AssemblyResolve(fun s e -> 
        //File.AppendAllText(@"c:\temp\ilbuilder.log", sprintf "--->resolving %s\n" e.Name)
        let a = typeof<int32>.Assembly
        //File.AppendAllText(@"c:\temp\ilbuilder.log", sprintf "--->code base: %s\n" a.CodeBase)
        let p = Path.Combine(Path.GetDirectoryName((Uri a.CodeBase).AbsolutePath), e.Name + ".dll")
        //File.AppendAllText(@"c:\temp\ilbuilder.log", sprintf "--->trying to load assembly: %s\n" p)
        Assembly.LoadFile(p))

    do containerType.DefineStaticParameters(
            staticParams,
            (fun typeName [| :? string as assemblies |] ->
                loader(typeName, assemblies)
            ))

    do base.AddNamespace(rootNamespace, [containerType])

[<TypeProviderAssembly>] 
do ()

