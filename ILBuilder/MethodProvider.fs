module Providers

open System.Collections.Generic
open System.Reflection

open Microsoft.FSharp.Core.CompilerServices

open ProviderImplementation
open ProviderImplementation.ProvidedTypes

[<TypeProvider>]
type MethodProvider() =
    inherit TypeProviderForNamespaces()

    let staticParams = 
        [
            ProvidedStaticParameter("Assembly", typeof<string>)
        ]
    let thisAssembly = Assembly.GetExecutingAssembly()
    let rootNamespace = "MethodsProvider"
    let containerType = ProvidedTypeDefinition(thisAssembly, rootNamespace, "Methods", None, IsErased = true)

    let lookup = Dictionary()

    let prettyPrintSig (m:MethodInfo) verbose = 
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
        
        let ps = m.GetParameters() 
        let prettyPrintParm (p:ParameterInfo) =
            if verbose then
                sprintf " %s:%s " p.Name (prettyPrintTy p.ParameterType)
            else
                prettyPrintTy p.ParameterType
        sprintf "%s : %s -> %s" m.Name (if ps.Length > 0 then System.String.Join("*", ps |> Array.map prettyPrintParm) else "unit") (prettyPrintTy m.ReturnType)

    let rec addNested (t : ProvidedTypeDefinition) parts (mi : MethodInfo) level = 
        match parts with
        | [] -> 
            let tok = mi.MetadataToken
            let name = prettyPrintSig mi false
            t.AddMemberDelayed (fun () -> 
                let p = ProvidedProperty(name, 
                                         typeof<MethodInfo>, 
                                         IsStatic = true, 
                                         GetterCode = fun args -> 
                                         <@@ typeof<int>.Assembly.ManifestModule.ResolveMethod(tok) @@>)
                p.AddXmlDoc(prettyPrintSig mi true)
                p)
        | p::parts ->
            t.AddMembersDelayed(fun () -> 
                let level = p :: level
                match lookup.TryGetValue(level) with
                | true, t -> 
                    addNested t parts mi level
                    []
                | _ -> 
                    let pt = ProvidedTypeDefinition(p, None)
                    lookup.Add(level, pt)
                    addNested pt parts mi level
                    [pt])


    let loader (typeName, assemblyName : string) = 
        let t = ProvidedTypeDefinition(thisAssembly, rootNamespace, typeName, None, IsErased = true)
        let assembly = Assembly.Load(assemblyName)
        let typesAndMethods = 
            assembly.ExportedTypes
            |> Seq.filter (fun ty -> ty <> typeof<System.Void>)
            |> Seq.collect (fun ty -> ty.GetMethods() |> Seq.map (fun mi -> ty, mi))
            |> Seq.toArray

        typesAndMethods
        |> Seq.iter (fun (ty, mi) ->
            let parts = (if ty.Namespace = null then [] else ty.Namespace.Split '.' |> Seq.toList) @ [ty.Name]
            addNested t parts mi [])
        t

    do containerType.DefineStaticParameters(
            staticParams,
            (fun typeName [| :? string as assembly |] ->
                printfn "--->typeName: %s" assembly
                loader(typeName, assembly)
            ))

    do base.AddNamespace(rootNamespace, [containerType])

[<TypeProviderAssembly>] 
do()
