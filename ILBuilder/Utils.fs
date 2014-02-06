namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open Microsoft.FSharp.Quotations

type Test() = 
    member x.JustGet
        with get() = 
            System.String([|'a'; 'b'|])
       
module internal Utils = 
    module Seq = 
        let toString sep (xs : _ seq) = String.Join(sep, xs)

    let (+>) f g x = 
        f x
        g x

    let toSystemType (t : Type) = System.Type.GetType(t.FullName)
    let ofType (u : Universe) (t : System.Type) = if t = null then null else u.Import(t)
    let ofTypes (u : Universe) (ts : System.Type[]) = ts |> Array.map (ofType u)
    let ofMethodInfo (u : Universe) (mi : System.Reflection.MethodInfo) =     
        let t = mi.DeclaringType |> ofType u
        let pts = mi.GetParameters()
        let ms = t.GetMethods()
        let umi = 
            match ms |> Seq.map (fun m -> m,m.GetParameters())
                        |> Seq.filter (fun (m,ps) -> m.Name = mi.Name && ps.Length = pts.Length)
                        |> Seq.toList with
            | [] -> failwithf "Type '%s' doesn't have a method with '%O'." t.FullName mi
            | [mps] -> fst mps
            | ms -> ms |> Seq.find (fun (m,ps) ->
                            (pts,ps) 
                            ||> Seq.zip
                            |> Seq.forall (fun (pt,p) -> 
                                if m.IsGenericMethodDefinition && p.ParameterType.IsGenericType
                                then u.Import(pt.ParameterType.GetGenericTypeDefinition()) = p.ParameterType.GetGenericTypeDefinition()
                                else u.Import(pt.ParameterType) = p.ParameterType))
                        |> fst
        if umi.ContainsGenericParameters && not(mi.GetGenericArguments() |> Seq.exists (fun arg -> arg.IsGenericParameter))
        then umi.MakeGenericMethod(mi.GetGenericArguments() |> Array.map (ofType u))
        else umi

    let ofConstructorInfo (u : Universe) (ci : System.Reflection.ConstructorInfo) =     
        let t = ci.DeclaringType |> ofType u
        let ps = ci.GetParameters() |> Array.map (fun p -> p.ParameterType |> ofType u)
        let ci = t.GetConstructor(ps)
        ci

    let ofFieldInfo (u : Universe) (fi : System.Reflection.FieldInfo) =
        let t = fi.DeclaringType |> ofType u
        let fi = t.GetField(fi.Name)
        fi

    let internal emitLabel opcode (label : Label) (u : Universe, il : ILGenerator) = il.Emit(opcode, label)
    let internal emit opcode (u : Universe, il : ILGenerator) = il.Emit(opcode)
    let internal emitByte opcode (arg : byte) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg)
    let internal emitSByte opcode (arg : sbyte) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg)
    let internal emitInt16 opcode (arg : int16) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
    let internal emitInt32 opcode (arg : int32) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
    let internal emitInt64 opcode (arg : int64) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
    let internal emitFloat opcode (arg : float32) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
    let internal emitDouble opcode (arg : double) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
    let internal emitType opcode (arg : System.Type) (u : Universe, il : ILGenerator) = il.Emit(opcode, ofType u arg)
    //let internal emitType opcode (cls : Type) (u : Universe, il : ILGenerator) = il.Emit(opcode, cls)
    let internal emitMethodInfo opcode mi (u : Universe, il : ILGenerator) = il.Emit(opcode, ofMethodInfo u mi) 
    let internal emitConstructorInfo opcode (con : ConstructorInfo) (u : Universe, il : ILGenerator) = il.Emit(opcode, con) 
    let internal emitFieldInfo opcode field (u : Universe, il : ILGenerator) = il.Emit(opcode, ofFieldInfo u field)
    let internal emitString opcode (str : String) (u : Universe, il : ILGenerator) = il.Emit(opcode, str) 
    let internal emitLocalBuilder opcode (local : LocalBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 
    let internal emitMethodBuilder opcode (local : MethodBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 
    let internal emitFieldBuilder opcode (local : FieldBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 

