namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open Microsoft.FSharp.Quotations

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

type internal Emit private () = 
    static member EmitOp(opcode) = 
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode)

    static member EmitLabel(opcode, label : Label) = 
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, label)

    static member EmitByte(opcode, arg : byte) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg)

    static member EmitSByte(opcode, arg : sbyte) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg)

    static member EmitInt16(opcode, arg : int16) = 
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg) 

    static member EmitInt32(opcode, arg : int32) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg) 

    static member EmitInt64(opcode, arg : int64) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg) 

    static member EmitFloat(opcode, arg : float32) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg) 

    static member EmitDouble(opcode, arg : double) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, arg) 

    static member EmitType(opcode, arg : System.Type) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, Utils.ofType u arg)

    static member EmitType(opcode, ty : Type) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, ty)

    static member EmitMethod(opcode, methodInfo) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, Utils.ofMethodInfo u methodInfo) 

    static member EmitMethod(opcode, methodInfo : MethodInfo) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, methodInfo) 

    static member EmitMethod(opcode, methodBuilder : MethodBuilder) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, methodBuilder)

    static member EmitConstructor(opcode, cons : System.Reflection.ConstructorInfo) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, Utils.ofConstructorInfo u cons) 

    static member EmitConstructor(opcode, cons : ConstructorInfo) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, cons) 

    static member EmitConstructor(opcode, cons : ConstructorBuilder) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, cons) 

    static member EmitField(opcode, field) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, Utils.ofFieldInfo u field)

    static member EmitField(opcode, fieldBuilder : FieldBuilder) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, fieldBuilder) 

    static member EmitString(opcode, str : String) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, str) 

    static member EmitLocal(opcode, local : LocalBuilder) =
        fun (u : Universe, il : ILGenerator) -> il.Emit(opcode, local)

