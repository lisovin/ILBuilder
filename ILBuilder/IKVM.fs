module IKVM

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open Microsoft.FSharp.Quotations

module Seq = 
    let toString sep (xs : _ seq) = String.Join(sep, xs)

let (+>) f g x = 
    f x
    g x

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
    let ci = t.GetMethod(ci.Name, ps)
    ci

let private emitLabel (opcode : OpCode) (label : Label) (u : Universe, il : ILGenerator) = il.Emit(opcode, label)
let private emit (opcode : OpCode) (u : Universe, il : ILGenerator) = il.Emit(opcode)
let private emitByte (opcode : OpCode) (arg : byte) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg)
let private emitSByte (opcode : OpCode) (arg : sbyte) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg)
let private emitInt16 (opcode : OpCode) (arg : int16) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
let private emitInt32 (opcode : OpCode) (arg : int32) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
let private emitInt64 (opcode : OpCode) (arg : int64) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
let private emitFloat (opcode : OpCode) (arg : float32) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
let private emitDouble (opcode : OpCode) (arg : double) (u : Universe, il : ILGenerator) = il.Emit(opcode, arg) 
let private emitType (opcode : OpCode) (cls : Type) (u : Universe, il : ILGenerator) = il.Emit(opcode, cls)
let private emitMethodInfo (opcode : OpCode) (meth : MethodInfo) (u : Universe, il : ILGenerator) = il.Emit(opcode, meth) 
let private emitConstructorInfo (opcode : OpCode) (con : ConstructorInfo) (u : Universe, il : ILGenerator) = il.Emit(opcode, con) 
let private emitFieldInfo (opcode : OpCode) (field : FieldInfo) (u : Universe, il : ILGenerator) = il.Emit(opcode, field)
let private emitString (opcode : OpCode) (str : String) (u : Universe, il : ILGenerator) = il.Emit(opcode, str) 
let private emitLocalBuilder (opcode : OpCode) (local : LocalBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 
let private emitMethodBuilder (opcode : OpCode) (local : MethodBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 
let private emitFieldBuilder (opcode : OpCode) (local : FieldBuilder) (u : Universe, il : ILGenerator) = il.Emit(opcode, local) 

type EmitBuilder() = 
    [<CustomOperation("add", MaintainsVariableSpace = true)>]
    member __.add(f) = f +> emit OpCodes.Add

    [<CustomOperation("add_ovf", MaintainsVariableSpace = true)>]
    member __.add_ovf(f) = f +> emit OpCodes.Add_Ovf

    [<CustomOperation("add_ovf_un", MaintainsVariableSpace = true)>]
    member __.add_ovf_un(f) = f +> emit OpCodes.Add_Ovf_Un

    [<CustomOperation("and", MaintainsVariableSpace = true)>]
    member __.and'(f) = f +> emit OpCodes.And

    [<CustomOperation("arglist", MaintainsVariableSpace = true)>]
    member __.arglist(f) = f +> emit OpCodes.Arglist

    [<CustomOperation("beginCatchBlock", MaintainsVariableSpace = true)>]
    member __.beginCatchBlock(exceptionType)(f) = fun (il : ILGenerator, u : Universe) -> il.BeginCatchBlock(u.Import(exceptionType))

    [<CustomOperation("beginExceptionBlock", MaintainsVariableSpace = true)>]
    member __.beginExceptionBlock()(f) = fun (il : ILGenerator) -> il.BeginExceptionBlock()

    [<CustomOperation("beq", MaintainsVariableSpace = true)>]
    member __.beq(f, label) = f +> emitLabel OpCodes.Beq label

    [<CustomOperation("beq_s", MaintainsVariableSpace = true)>]
    member __.beq_s(f, label) = f +> emitLabel OpCodes.Beq_S label 

    [<CustomOperation("bge", MaintainsVariableSpace = true)>]
    member __.bge(f, label) = f +> emitLabel OpCodes.Bge label 

    [<CustomOperation("bge_s", MaintainsVariableSpace = true)>]
    member __.bge_s(f, label) = f +> emitLabel OpCodes.Bge_S label 

    [<CustomOperation("bge_un", MaintainsVariableSpace = true)>]
    member __.bge_un(f, label) = f +> emitLabel OpCodes.Bge_Un label 

    [<CustomOperation("bge_un_s", MaintainsVariableSpace = true)>]
    member __.bge_un_s(f, label) = f +> emitLabel OpCodes.Bge_Un_S label 

    [<CustomOperation("bgt", MaintainsVariableSpace = true)>]
    member __.bgt(f, label) = f +> emitLabel OpCodes.Bgt label 

    [<CustomOperation("bgt_s", MaintainsVariableSpace = true)>]
    member __.bgt_s(f, label) = f +> emitLabel OpCodes.Bgt_S label 

    [<CustomOperation("bgt_un", MaintainsVariableSpace = true)>]
    member __.bgt_un(f, label) = f +> emitLabel OpCodes.Bgt_Un label 

    [<CustomOperation("bgt_un_s", MaintainsVariableSpace = true)>]
    member __.bgt_un_s(f, label) = f +> emitLabel OpCodes.Bgt_Un_S label 

    [<CustomOperation("ble", MaintainsVariableSpace = true)>]
    member __.ble(f, label) = f +> emitLabel OpCodes.Ble label 

    [<CustomOperation("ble_s", MaintainsVariableSpace = true)>]
    member __.ble_s(f, label) = f +> emitLabel OpCodes.Ble_S label 

    [<CustomOperation("ble_un", MaintainsVariableSpace = true)>]
    member __.ble_un(f, label) = f +> emitLabel OpCodes.Ble_Un label 

    [<CustomOperation("ble_un_s", MaintainsVariableSpace = true)>]
    member __.ble_un_s(f, label) = f +> emitLabel OpCodes.Ble_Un_S label 

    [<CustomOperation("blt", MaintainsVariableSpace = true)>]
    member __.blt(f, label) = f +> emitLabel OpCodes.Blt label 

    [<CustomOperation("blt_s", MaintainsVariableSpace = true)>]
    member __.blt_s(f, label) = f +> emitLabel OpCodes.Blt_S label 

    [<CustomOperation("blt_un", MaintainsVariableSpace = true)>]
    member __.blt_un(f, label) = f +> emitLabel OpCodes.Blt_Un label 

    [<CustomOperation("blt_un_s", MaintainsVariableSpace = true)>]
    member __.blt_un_s(f, label) = f +> emitLabel OpCodes.Blt_Un_S label 

    [<CustomOperation("bne_un", MaintainsVariableSpace = true)>]
    member __.bne_un(f, label) = f +> emitLabel OpCodes.Bne_Un label 

    [<CustomOperation("bne_un_s", MaintainsVariableSpace = true)>]
    member __.bne_un_s(f, label) = f +> emitLabel OpCodes.Bne_Un_S label 

    [<CustomOperation("box", MaintainsVariableSpace = true)>]
    member __.box (f, typ : System.Type) = fun (il : ILGenerator, u : Universe) -> il.Emit(OpCodes.Box, u.Import(typ)) 

    [<CustomOperation("br", MaintainsVariableSpace = true)>]
    member __.br(f, label) = f +> emitLabel OpCodes.Br label 

    [<CustomOperation("brfalse", MaintainsVariableSpace = true)>]
    member __.brfalse(f, label) = f +> emitLabel OpCodes.Brfalse label 

    [<CustomOperation("brfalse_s", MaintainsVariableSpace = true)>]
    member __.brfalse_s (f, label : Label) = f +> emitLabel OpCodes.Brfalse_S label

    [<CustomOperation("brtrue", MaintainsVariableSpace = true)>]
    member __.brtrue(f, label) = f +> emitLabel OpCodes.Brtrue label 

    [<CustomOperation("brtrue_s", MaintainsVariableSpace = true)>]
    member __.brtrue_s(f, label) = f +> emitLabel OpCodes.Brtrue_S label 

    [<CustomOperation("br_s", MaintainsVariableSpace = true)>]
    member __.br_s(f, label) = f +> emitLabel OpCodes.Br_S label 

    [<CustomOperation("call", MaintainsVariableSpace = true)>]
    member __.call(f, targetType : Type, methodName, [<ParamArray>] optionalParameterTypes : Type[]) = 
        if targetType = null 
        then ArgumentException("targetType") |> raise
        else
            let mi = targetType.GetMethod(methodName, optionalParameterTypes)
            if mi = null
            then InvalidOperationException(sprintf "Method '%s' with parameters '%s' is not defined on type '%s'" methodName targetType.Name (optionalParameterTypes |> Seq.map (fun t -> t.Name) |> Seq.toString ", ")) |> raise
            else
            f +> emitMethodInfo OpCodes.Call mi
    
    [<CustomOperation("call", MaintainsVariableSpace = true)>]
    member __.call (f, methodInfo : System.Reflection.MethodInfo)=     
        if methodInfo = null then ArgumentNullException("methodInfo", "Method info cannot be null.") |> raise
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            emitMethodInfo OpCodes.Call (ofMethodInfo u methodInfo)

    [<CustomOperation("callvirt", MaintainsVariableSpace = true)>]
    member __.callvirt (f, methodInfo : System.Reflection.MethodInfo) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            emitMethodInfo OpCodes.Callvirt (ofMethodInfo u methodInfo) (u,il)

    [<CustomOperation("castclass", MaintainsVariableSpace = true)>]
    member __.castclass (f, typ : Type) = f +> emitType OpCodes.Castclass typ 

    [<CustomOperation("castclass", MaintainsVariableSpace = true)>]
    member __.castclass (f, typ : System.Type) = 
        fun (u : Universe, il : ILGenerator) -> 
            let g = __.castclass(f, ofType u typ)
            g(u, il)

    [<CustomOperation("ceq", MaintainsVariableSpace = true)>]
    member __.ceq(f) = f +> emit OpCodes.Ceq

    [<CustomOperation("cgt", MaintainsVariableSpace = true)>]
    member __.cgt(f) = f +> emit OpCodes.Cgt

    [<CustomOperation("cgt_un", MaintainsVariableSpace = true)>]
    member __.cgt_un(f) = f +> emit OpCodes.Cgt_Un

    [<CustomOperation("ckfinite", MaintainsVariableSpace = true)>]
    member __.ckfinite(f) = f +> emit OpCodes.Ckfinite

    [<CustomOperation("clt", MaintainsVariableSpace = true)>]
    member __.clt(f) = f +> emit OpCodes.Clt

    [<CustomOperation("clt_un", MaintainsVariableSpace = true)>]
    member __.clt_un(f) = f +> emit OpCodes.Clt_Un

    [<CustomOperation("constrained", MaintainsVariableSpace = true)>]
    member __.constrained (f, typ : Type) = f +> emitType OpCodes.Constrained typ 

    [<CustomOperation("cpobj", MaintainsVariableSpace = true)>]
    member __.cpobj (f, typ : Type) = f +> emitType OpCodes.Cpobj typ 

    [<CustomOperation("declareLocal", MaintainsVariableSpace = true)>]
    member __.declareLocal(d, t : System.Type) = fun (il : ILGenerator, u : Universe) -> il.DeclareLocal(ofType u t)

    [<CustomOperation("declareLocal", MaintainsVariableSpace = true)>]
    member __.declareLocal(f, tb : TypeBuilder) = fun (il : ILGenerator) -> il.DeclareLocal(tb)

    [<CustomOperation("defineLabel", MaintainsVariableSpace = true)>]
    member __.defineLabel(f) = 
        fun (u : Universe, il : ILGenerator) ->
            f(u, il)
            il.DefineLabel()

    [<CustomOperation("endExceptionBlock", MaintainsVariableSpace = true)>]
    member __.endExceptionBlock(f) = fun (il : ILGenerator) -> il.EndExceptionBlock()

    [<CustomOperation("initobj", MaintainsVariableSpace = true)>]
    member __.initobj (typ : Type)(f) = f +> emitType OpCodes.Initobj typ 

    [<CustomOperation("isinst", MaintainsVariableSpace = true)>]
    member __.isinst (typ : Type)(f) = f +> emitType OpCodes.Isinst typ 

    [<CustomOperation("jmp", MaintainsVariableSpace = true)>]
    member __.jmp (methodInfo : MethodInfo)(f) = f +> emitMethodInfo OpCodes.Jmp methodInfo 

    [<CustomOperation("ldarg_s", MaintainsVariableSpace = true)>]
    member __.ldarg_s(f, index: byte) = f +> emitByte OpCodes.Ldarg_S index

    [<CustomOperation("ldarg_s", MaintainsVariableSpace = true)>]
    member __.ldarg_s(f, index: int16) = f +> emitInt16 OpCodes.Ldarg index

    [<CustomOperation("ldarg", MaintainsVariableSpace = true)>]
    member __.ldarg(f, index) =                         
        match index with
        |0 -> __.ldarg_0 f
        |1 -> __.ldarg_1 f
        |2 -> __.ldarg_2 f
        |3 -> __.ldarg_3 f
        |n when n <= (int)Byte.MaxValue -> __.ldarg_s(f, byte n)
        |n when n <= (int)Int16.MaxValue -> __.ldarg(f, int16 n)
        |_ -> ArgumentOutOfRangeException("index") |> raise

    [<CustomOperation("ldarg", MaintainsVariableSpace = true)>]
    member __.ldarg(f, index : int16) = f +> emitInt16 OpCodes.Ldarg index 

    [<CustomOperation("ldarga", MaintainsVariableSpace = true)>]
    member __.ldarga(f, index : int16) = f +> emitInt16 OpCodes.Ldarga index 

    [<CustomOperation("ldc_bool", MaintainsVariableSpace = true)>]
    member __.ldc_bool (b : bool)(f) = f +> emit (if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)

    [<CustomOperation("ldc_i4", MaintainsVariableSpace = true)>]
    member __.ldc_i4(f, num : int) = f +> emitInt32 OpCodes.Ldc_I4 num 

    [<CustomOperation("ldc_i4_s", MaintainsVariableSpace = true)>]
    member __.ldc_i4_s(f, num : sbyte) = f +> emitSByte OpCodes.Ldc_I4_S num 

    [<CustomOperation("ldc_i4_s", MaintainsVariableSpace = true)>]
    member __.ldc_i4_s (f, ch : char) = fun (il : ILGenerator) -> il.Emit(OpCodes.Ldc_I4_S, sbyte ch)

    [<CustomOperation("ldc_i8", MaintainsVariableSpace = true)>]
    member __.ldc_i8(f, num : int64) = f +> emitInt64 OpCodes.Ldc_I8 num 

    [<CustomOperation("ldc_r4", MaintainsVariableSpace = true)>]
    member __.ldc_r4 (f, num : float32) = f +> emitFloat OpCodes.Ldc_R4 num 

    [<CustomOperation("ldc_r8", MaintainsVariableSpace = true)>]
    member __.ldc_r8 (f, num : double) = f +> emitDouble OpCodes.Ldc_R8 num 

    [<CustomOperation("ldelema", MaintainsVariableSpace = true)>]
    member __.ldelema (f, typ : Type) = f +> emitType OpCodes.Ldelema typ 

    [<CustomOperation("ldfld", MaintainsVariableSpace = true)>]
    member __.ldfld (f, fieldInfo : FieldInfo) = f +> emitFieldInfo OpCodes.Ldfld fieldInfo 

    [<CustomOperation("ldflda", MaintainsVariableSpace = true)>]
    member __.ldflda (fieldInfo : FieldInfo)(f) = f +> emitFieldInfo OpCodes.Ldflda fieldInfo 

    [<CustomOperation("ldftn", MaintainsVariableSpace = true)>]
    member __.ldftn (f, methodBuilder : MethodBuilder) = f +> emitMethodBuilder OpCodes.Ldftn methodBuilder 

    [<CustomOperation("ldftn", MaintainsVariableSpace = true)>]
    member __.ldftn (f, methodInfo : System.Reflection.MethodInfo) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            emitMethodInfo OpCodes.Ldftn (ofMethodInfo u methodInfo) 

    [<CustomOperation("ldloc", MaintainsVariableSpace = true)>]
    member __.ldloc (f, index : int16) = f +> emitInt16 OpCodes.Ldloc index 

    [<CustomOperation("ldloc", MaintainsVariableSpace = true)>]
    member __.ldloc (f, localBuilder : LocalBuilder) = f +> emitLocalBuilder OpCodes.Ldloc localBuilder 

    [<CustomOperation("ldloca", MaintainsVariableSpace = true)>]
    member __.ldloca (f, index : int16) = f +> emitInt16 OpCodes.Ldloca index 

    [<CustomOperation("ldloca_s", MaintainsVariableSpace = true)>]
    member __.ldloca_s (f, index : byte) = f +> emitByte OpCodes.Ldloca_S index 

    [<CustomOperation("ldloca", MaintainsVariableSpace = true)>]
    member __.ldloca (f, local : LocalBuilder) = f +> emitLocalBuilder OpCodes.Ldloca local 

    [<CustomOperation("ldloc_s", MaintainsVariableSpace = true)>]
    member __.ldloc_s (f, index : byte) = f +> emitByte OpCodes.Ldloca_S index 

    [<CustomOperation("ldobj", MaintainsVariableSpace = true)>]
    member __.ldobj (typ : Type)(f) = f +> emitType OpCodes.Ldobj typ 

    [<CustomOperation("ldsfld", MaintainsVariableSpace = true)>]
    member __.ldsfld (f, fieldBuilder : FieldBuilder) = f +> emitFieldBuilder OpCodes.Ldsfld fieldBuilder

    [<CustomOperation("ldsfld", MaintainsVariableSpace = true)>]
    member __.ldsfld (f, fieldInfo : System.Reflection.FieldInfo) = 
        fun (il : ILGenerator, u : Universe) -> 
            let declaringType = fieldInfo.DeclaringType |> u.Import
            let fi = declaringType.GetField(fieldInfo.Name)
            emitFieldInfo OpCodes.Ldsfld fi

    [<CustomOperation("ldsflda", MaintainsVariableSpace = true)>]
    member __.ldsflda (f, fieldInfo : FieldInfo) = f +> emitFieldInfo OpCodes.Ldsflda fieldInfo 

    [<CustomOperation("ldstr", MaintainsVariableSpace = true)>]
    member __.ldstr (f, str : string)= f +> emitString OpCodes.Ldstr str 

    [<CustomOperation("ldtoken", MaintainsVariableSpace = true)>]
    member __.ldtoken (f, methodInfo : MethodInfo) = f +> emitMethodInfo OpCodes.Ldtoken methodInfo 

    [<CustomOperation("ldtoken", MaintainsVariableSpace = true)>]
    member __.ldtoken (f, fieldInfo : FieldInfo) = f +> emitFieldInfo OpCodes.Ldtoken fieldInfo 

    [<CustomOperation("ldtoken", MaintainsVariableSpace = true)>]
    member __.ldtoken (f, typ : Type) = f +> emitType OpCodes.Ldtoken typ 

    [<CustomOperation("ldtoken", MaintainsVariableSpace = true)>]
    member __.ldtoken (f, typ : System.Type) = fun (il : ILGenerator, u : Universe) -> il.Emit(OpCodes.Ldtoken, u.Import(typ)) 

    [<CustomOperation("ldvirtftn", MaintainsVariableSpace = true)>]
    member __.ldvirtftn (f, methodInfo : MethodInfo) = f +> emitMethodInfo OpCodes.Ldvirtftn methodInfo 

    [<CustomOperation("leave", MaintainsVariableSpace = true)>]
    member __.leave(f, label) = f +> emitLabel OpCodes.Leave label 

    [<CustomOperation("leave_s", MaintainsVariableSpace = true)>]
    member __.leave_s(label, f) = f +> emitLabel OpCodes.Leave_S label 

    [<CustomOperation("markLabel", MaintainsVariableSpace = true)>]
    member __.markLabel(f, label : Label) = f +> fun (_, il : ILGenerator) -> il.MarkLabel(label)

    [<CustomOperation("mkrefany", MaintainsVariableSpace = true)>]
    member __.mkrefany (typ : System.Type)(f) = fun (il : ILGenerator, u : Universe) -> il.Emit(OpCodes.Mkrefany, u.Import(typ)) 

    [<CustomOperation("newarr", MaintainsVariableSpace = true)>]
    member __.newarr (elType : System.Type)(f) = fun (il : ILGenerator, u : Universe) -> il.Emit(OpCodes.Newarr, u.Import(elType)) 

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj (f, constructorInfo : System.Reflection.ConstructorInfo) = 
        fun (il : ILGenerator, u : Universe) -> 
            let t = constructorInfo.DeclaringType |> ofType u
            let pts = constructorInfo.GetParameters() |> Array.map (fun p -> p.ParameterType |> ofType u)                   
            let ci = t.GetConstructor(pts)
            il.Emit(OpCodes.Newobj, ci)

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj<'t when 't : (new : unit -> 't)>(f) = 
        fun (il : ILGenerator, u : Universe) -> 
            f +> emitConstructorInfo OpCodes.Newobj (u.Import(typeof<'t>).GetConstructor([||]))

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj(f, t : Type) = 
        f +> emitConstructorInfo OpCodes.Newobj (t.GetConstructor([||]))

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj(f, t : System.Type) = __.newobj(f, t.GetConstructor([||]))

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj(f, cb : ConstructorBuilder) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            il.Emit(OpCodes.Newobj, cb) 

    [<CustomOperation("refanyval", MaintainsVariableSpace = true)>]
    member __.refanyval (typ : Type)(f) = f +> emitType OpCodes.Refanyval typ 

    [<CustomOperation("ret", MaintainsVariableSpace = true)>]
    member __.ret(f) = f +> emit OpCodes.Ret 

    [<CustomOperation("sizeof", MaintainsVariableSpace = true)>]
    member __.sizeof (typ : Type)(f) = f +> emitType OpCodes.Sizeof typ 

    [<CustomOperation("starg", MaintainsVariableSpace = true)>]
    member __.starg (index : int16)(f) = f +> emitInt16 OpCodes.Starg index 

    [<CustomOperation("starg_s", MaintainsVariableSpace = true)>]
    member __.starg_s (index : byte)(f) = f +> emitByte OpCodes.Starg_S index 

    [<CustomOperation("stfld__FieldBuilder", MaintainsVariableSpace = true)>]
    member __.stfld__FieldBuilde (f, fieldBuilder : FieldBuilder) = f +> emitFieldBuilder OpCodes.Stfld fieldBuilder 

    [<CustomOperation("stfld__FieldInfo", MaintainsVariableSpace = true)>]
    member __.stfld__FieldInfo (f, fieldInfo : FieldInfo) = f +> emitFieldInfo OpCodes.Stfld fieldInfo 

    [<CustomOperation("stloc", MaintainsVariableSpace = true)>]
    member __.stloc (local : LocalBuilder)(f) = f +> emitLocalBuilder OpCodes.Stloc local 

    [<CustomOperation("stloc_s", MaintainsVariableSpace = true)>]
    member __.stloc_s (local : LocalBuilder)(f) = f +> emitLocalBuilder OpCodes.Stloc_S local 

    [<CustomOperation("stobj", MaintainsVariableSpace = true)>]
    member __.stobj (typ : Type)(f) = f +> emitType OpCodes.Stobj typ 

    [<CustomOperation("stsfld", MaintainsVariableSpace = true)>]
    member __.stsfld (fieldInfo : FieldInfo)(f) = f +> emitFieldInfo OpCodes.Stsfld fieldInfo 

    [<CustomOperation("thisType", MaintainsVariableSpace = true)>]
    member __.thisType(f) = fun (il : ILGenerator, mb : MethodBuilder) -> mb.DeclaringType

    [<CustomOperation("unaligned", MaintainsVariableSpace = true)>]
    member __.unaligned(f, label) = f +> emitLabel OpCodes.Unaligned label 

    [<CustomOperation("unaligned", MaintainsVariableSpace = true)>]
    member __.unaligned (f, addr : int64) = f +> emitInt64 OpCodes.Unaligned addr 

    [<CustomOperation("unbox", MaintainsVariableSpace = true)>]
    member __.unbox (typ : Type)(f) = f +> emitType OpCodes.Unbox typ 

    [<CustomOperation("unbox_any", MaintainsVariableSpace = true)>]
    member __.unbox_any (typ : System.Type)(f) = fun (il : ILGenerator, u : Universe) -> il.Emit(OpCodes.Unbox_Any, u.Import(typ)) 

    [<CustomOperation("conv_i", MaintainsVariableSpace = true)>]
    member __.conv_i(f) = f +> emit OpCodes.Conv_I

    [<CustomOperation("conv_i1", MaintainsVariableSpace = true)>]
    member __.conv_i1(f) = f +> emit OpCodes.Conv_I1

    [<CustomOperation("conv_i2", MaintainsVariableSpace = true)>]
    member __.conv_i2(f) = f +> emit OpCodes.Conv_I2

    [<CustomOperation("conv_i4", MaintainsVariableSpace = true)>]
    member __.conv_i4(f) = f +> emit OpCodes.Conv_I4

    [<CustomOperation("conv_i8", MaintainsVariableSpace = true)>]
    member __.conv_i8(f) = f +> emit OpCodes.Conv_I8

    [<CustomOperation("conv_ovf_i", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i(f) = f +> emit OpCodes.Conv_Ovf_I

    [<CustomOperation("conv_ovf_i1", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i1(f) = f +> emit OpCodes.Conv_Ovf_I1

    [<CustomOperation("conv_ovf_i1_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i1_un(f) = f +> emit OpCodes.Conv_Ovf_I1_Un

    [<CustomOperation("conv_ovf_i2", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i2(f) = f +> emit OpCodes.Conv_Ovf_I2

    [<CustomOperation("conv_ovf_i2_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i2_un(f) = f +> emit OpCodes.Conv_Ovf_I2_Un

    [<CustomOperation("conv_ovf_i4", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i4(f) = f +> emit OpCodes.Conv_Ovf_I2_Un

    [<CustomOperation("conv_ovf_i4_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i4_un(f) = f +> emit OpCodes.Conv_Ovf_I4_Un

    [<CustomOperation("conv_ovf_i8", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i8(f) = f +> emit OpCodes.Conv_Ovf_I8

    [<CustomOperation("conv_ovf_i8_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i8_un(f) = f +> emit OpCodes.Conv_Ovf_I8_Un

    [<CustomOperation("conv_ovf_i_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_i_un(f) = f +> emit OpCodes.Conv_Ovf_I_Un

    [<CustomOperation("conv_ovf_u", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u(f) = f +> emit OpCodes.Conv_Ovf_U

    [<CustomOperation("conv_ovf_u1", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u1(f) = f +> emit OpCodes.Conv_Ovf_U1

    [<CustomOperation("conv_ovf_u1_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u1_un(f) = f +> emit OpCodes.Conv_Ovf_U1_Un

    [<CustomOperation("conv_ovf_u2", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u2(f) = f +> emit OpCodes.Conv_Ovf_U2

    [<CustomOperation("conv_ovf_u2_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u2_un(f) = f +> emit OpCodes.Conv_Ovf_U2_Un

    [<CustomOperation("conv_ovf_u4", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u4(f) = f +> emit OpCodes.Conv_Ovf_U4

    [<CustomOperation("conv_ovf_u4_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u4_un(f) = f +> emit OpCodes.Conv_Ovf_U4_Un

    [<CustomOperation("conv_ovf_u8", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u8(f) = f +> emit OpCodes.Conv_Ovf_U8

    [<CustomOperation("conv_ovf_u8_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u8_un(f) = f +> emit OpCodes.Conv_Ovf_U8_Un

    [<CustomOperation("conv_ovf_u_un", MaintainsVariableSpace = true)>]
    member __.conv_ovf_u_un(f) = f +> emit OpCodes.Conv_Ovf_U_Un

    [<CustomOperation("conv_r4", MaintainsVariableSpace = true)>]
    member __.conv_r4(f) = f +> emit OpCodes.Conv_R4

    [<CustomOperation("conv_r8", MaintainsVariableSpace = true)>]
    member __.conv_r8(f) = f +> emit OpCodes.Conv_R8

    [<CustomOperation("conv_r_un", MaintainsVariableSpace = true)>]
    member __.conv_r_un(f) = f +> emit OpCodes.Conv_R_Un

    [<CustomOperation("conv_u", MaintainsVariableSpace = true)>]
    member __.conv_u(f) = f +> emit OpCodes.Conv_U

    [<CustomOperation("conv_u1", MaintainsVariableSpace = true)>]
    member __.conv_u1(f) = f +> emit OpCodes.Conv_U1

    [<CustomOperation("conv_u2", MaintainsVariableSpace = true)>]
    member __.conv_u2(f) = f +> emit OpCodes.Conv_U2

    [<CustomOperation("conv_u4", MaintainsVariableSpace = true)>]
    member __.conv_u4(f) = f +> emit OpCodes.Conv_U4

    [<CustomOperation("conv_u8", MaintainsVariableSpace = true)>]
    member __.conv_u8(f) = f +> emit OpCodes.Conv_U8

    [<CustomOperation("cpblk", MaintainsVariableSpace = true)>]
    member __.cpblk(f) = f +> emit OpCodes.Cpblk

    [<CustomOperation("div", MaintainsVariableSpace = true)>]
    member __.div(f) = f +> emit OpCodes.Div

    [<CustomOperation("div_un", MaintainsVariableSpace = true)>]
    member __.div_un(f) = f +> emit OpCodes.Div_Un

    [<CustomOperation("dup", MaintainsVariableSpace = true)>]
    member __.dup(f) = f +> emit OpCodes.Dup

    [<CustomOperation("endfilter", MaintainsVariableSpace = true)>]
    member __.endfilter(f) = f +> emit OpCodes.Endfilter

    [<CustomOperation("endfinally", MaintainsVariableSpace = true)>]
    member __.endfinally(f) = f +> emit OpCodes.Endfinally

    [<CustomOperation("initblk", MaintainsVariableSpace = true)>]
    member __.initblk(f) = f +> emit OpCodes.Initblk

    [<CustomOperation("ldarg_0", MaintainsVariableSpace = true)>]
    member __.ldarg_0(f) = f +> emit OpCodes.Ldarg_0

    [<CustomOperation("ldarg_1", MaintainsVariableSpace = true)>]
    member __.ldarg_1(f) = f +> emit OpCodes.Ldarg_1

    [<CustomOperation("ldarg_2", MaintainsVariableSpace = true)>]
    member __.ldarg_2(f) = f +> emit OpCodes.Ldarg_2

    [<CustomOperation("ldarg_3", MaintainsVariableSpace = true)>]
    member __.ldarg_3(f) = f +> emit OpCodes.Ldarg_3

    [<CustomOperation("ldc_i4_0", MaintainsVariableSpace = true)>]
    member __.ldc_i4_0(f) = f +> emit OpCodes.Ldc_I4_0

    [<CustomOperation("ldc_i4_1", MaintainsVariableSpace = true)>]
    member __.ldc_i4_1(f) = f +> emit OpCodes.Ldc_I4_1

    [<CustomOperation("ldc_i4_2", MaintainsVariableSpace = true)>]
    member __.ldc_i4_2(f) = f +> emit OpCodes.Ldc_I4_2

    [<CustomOperation("ldc_i4_3", MaintainsVariableSpace = true)>]
    member __.ldc_i4_3(f) = f +> emit OpCodes.Ldc_I4_3

    [<CustomOperation("ldc_i4_4", MaintainsVariableSpace = true)>]
    member __.ldc_i4_4(f) = f +> emit OpCodes.Ldc_I4_4

    [<CustomOperation("ldc_i4_5", MaintainsVariableSpace = true)>]
    member __.ldc_i4_5(f) = f +> emit OpCodes.Ldc_I4_5

    [<CustomOperation("ldc_i4_6", MaintainsVariableSpace = true)>]
    member __.ldc_i4_6(f) = f +> emit OpCodes.Ldc_I4_6

    [<CustomOperation("ldc_i4_7", MaintainsVariableSpace = true)>]
    member __.ldc_i4_7(f) = f +> emit OpCodes.Ldc_I4_7

    [<CustomOperation("ldc_i4_8", MaintainsVariableSpace = true)>]
    member __.ldc_i4_8(f) = f +> emit OpCodes.Ldc_I4_8

    [<CustomOperation("ldc_i4_m1", MaintainsVariableSpace = true)>]
    member __.ldc_i4_m1(f) = f +> emit OpCodes.Ldc_I4_M1

    [<CustomOperation("ldelem_i", MaintainsVariableSpace = true)>]
    member __.ldelem_i(f) = f +> emit OpCodes.Ldelem_I

    [<CustomOperation("ldelem_i1", MaintainsVariableSpace = true)>]
    member __.ldelem_i1(f) = f +> emit OpCodes.Ldelem_I1

    [<CustomOperation("ldelem_i2", MaintainsVariableSpace = true)>]
    member __.ldelem_i2(f) = f +> emit OpCodes.Ldelem_I2

    [<CustomOperation("ldelem_i4", MaintainsVariableSpace = true)>]
    member __.ldelem_i4(f) = f +> emit OpCodes.Ldelem_I4

    [<CustomOperation("ldelem_i8", MaintainsVariableSpace = true)>]
    member __.ldelem_i8(f) = f +> emit OpCodes.Ldelem_I8

    [<CustomOperation("ldelem_r4", MaintainsVariableSpace = true)>]
    member __.ldelem_r4(f) = f +> emit OpCodes.Ldelem_R4

    [<CustomOperation("ldelem_r8", MaintainsVariableSpace = true)>]
    member __.ldelem_r8(f) = f +> emit OpCodes.Ldelem_R8

    [<CustomOperation("ldelem_ref", MaintainsVariableSpace = true)>]
    member __.ldelem_ref(f) = f +> emit OpCodes.Ldelem_Ref

    [<CustomOperation("ldelem_u1", MaintainsVariableSpace = true)>]
    member __.ldelem_u1(f) = f +> emit OpCodes.Ldelem_U1

    [<CustomOperation("ldelem_u2", MaintainsVariableSpace = true)>]
    member __.ldelem_u2(f) = f +> emit OpCodes.Ldelem_U2

    [<CustomOperation("ldelem_u4", MaintainsVariableSpace = true)>]
    member __.ldelem_u4(f) = f +> emit OpCodes.Ldelem_U4

    [<CustomOperation("ldind_i", MaintainsVariableSpace = true)>]
    member __.ldind_i(f) = f +> emit OpCodes.Ldind_I

    [<CustomOperation("ldind_i1", MaintainsVariableSpace = true)>]
    member __.ldind_i1(f) = f +> emit OpCodes.Ldind_I1

    [<CustomOperation("ldind_i2", MaintainsVariableSpace = true)>]
    member __.ldind_i2(f) = f +> emit OpCodes.Ldind_I2

    [<CustomOperation("ldind_i4", MaintainsVariableSpace = true)>]
    member __.ldind_i4(f) = f +> emit OpCodes.Ldind_I4

    [<CustomOperation("ldind_i8", MaintainsVariableSpace = true)>]
    member __.ldind_i8(f) = f +> emit OpCodes.Ldind_I8

    [<CustomOperation("ldind_r4", MaintainsVariableSpace = true)>]
    member __.ldind_r4(f) = f +> emit OpCodes.Ldind_R4

    [<CustomOperation("ldind_r8", MaintainsVariableSpace = true)>]
    member __.ldind_r8(f) = f +> emit OpCodes.Ldind_R8

    [<CustomOperation("ldind_ref", MaintainsVariableSpace = true)>]
    member __.ldind_ref(f) = f +> emit OpCodes.Ldind_Ref

    [<CustomOperation("ldind_u1", MaintainsVariableSpace = true)>]
    member __.ldind_u1(f) = f +> emit OpCodes.Ldind_U1

    [<CustomOperation("ldind_u2", MaintainsVariableSpace = true)>]
    member __.ldind_u2(f) = f +> emit OpCodes.Ldind_U2

    [<CustomOperation("ldind_u4", MaintainsVariableSpace = true)>]
    member __.ldind_u4(f) = f +> emit OpCodes.Ldind_U4

    [<CustomOperation("ldlen", MaintainsVariableSpace = true)>]
    member __.ldlen(f) = f +> emit OpCodes.Ldlen

    [<CustomOperation("ldloc_0", MaintainsVariableSpace = true)>]
    member __.ldloc_0(f) = f +> emit OpCodes.Ldloc_0

    [<CustomOperation("ldloc_1", MaintainsVariableSpace = true)>]
    member __.ldloc_1(f) = f +> emit OpCodes.Ldloc_1

    [<CustomOperation("ldloc_2", MaintainsVariableSpace = true)>]
    member __.ldloc_2(f) = f +> emit OpCodes.Ldloc_2

    [<CustomOperation("ldloc_3", MaintainsVariableSpace = true)>]
    member __.ldloc_3(f) = f +> emit OpCodes.Ldloc_3

    [<CustomOperation("ldnull", MaintainsVariableSpace = true)>]
    member __.ldnull(f) = f +> emit OpCodes.Ldnull

    [<CustomOperation("localloc", MaintainsVariableSpace = true)>]
    member __.localloc(f) = f +> emit OpCodes.Localloc

    [<CustomOperation("mul", MaintainsVariableSpace = true)>]
    member __.mul(f) = f +> emit OpCodes.Mul

    [<CustomOperation("mul_ovf", MaintainsVariableSpace = true)>]
    member __.mul_ovf(f) = f +> emit OpCodes.Mul_Ovf

    [<CustomOperation("mul_ovf_un", MaintainsVariableSpace = true)>]
    member __.mul_ovf_un(f) = f +> emit OpCodes.Mul_Ovf_Un

    [<CustomOperation("neg", MaintainsVariableSpace = true)>]
    member __.neg(f) = f +> emit OpCodes.Neg

    [<CustomOperation("nop", MaintainsVariableSpace = true)>]
    member __.nop(f) = f +> emit OpCodes.Nop

    [<CustomOperation("not", MaintainsVariableSpace = true)>]
    member __.not'(f) = f +> emit OpCodes.Not

    [<CustomOperation("or", MaintainsVariableSpace = true)>]
    member __.or'(f) = f +> emit OpCodes.Or

    [<CustomOperation("pop", MaintainsVariableSpace = true)>]
    member __.pop(f) = f +> emit OpCodes.Pop

    [<CustomOperation("refanytype", MaintainsVariableSpace = true)>]
    member __.refanytype(f) = f +> emit OpCodes.Refanytype

    [<CustomOperation("rem", MaintainsVariableSpace = true)>]
    member __.rem(f) = f +> emit OpCodes.Rem

    [<CustomOperation("rem_un", MaintainsVariableSpace = true)>]
    member __.rem_un(f) = f +> emit OpCodes.Rem_Un

    [<CustomOperation("rethrow", MaintainsVariableSpace = true)>]
    member __.rethrow(f) = f +> emit OpCodes.Rethrow

    [<CustomOperation("shl", MaintainsVariableSpace = true)>]
    member __.shl(f) = f +> emit OpCodes.Shl

    [<CustomOperation("shr", MaintainsVariableSpace = true)>]
    member __.shr(f) = f +> emit OpCodes.Shr

    [<CustomOperation("shr_un", MaintainsVariableSpace = true)>]
    member __.shr_un(f) = f +> emit OpCodes.Shr_Un

    [<CustomOperation("stelem_i", MaintainsVariableSpace = true)>]
    member __.stelem_i(f) = f +> emit OpCodes.Stelem_I

    [<CustomOperation("stelem_i1", MaintainsVariableSpace = true)>]
    member __.stelem_i1(f) = f +> emit OpCodes.Stelem_I1

    [<CustomOperation("stelem_i2", MaintainsVariableSpace = true)>]
    member __.stelem_i2(f) = f +> emit OpCodes.Stelem_I2

    [<CustomOperation("stelem_i4", MaintainsVariableSpace = true)>]
    member __.stelem_i4(f) = f +> emit OpCodes.Stelem_I4

    [<CustomOperation("stelem_i8", MaintainsVariableSpace = true)>]
    member __.stelem_i8(f) = f +> emit OpCodes.Stelem_I8

    [<CustomOperation("stelem_r4", MaintainsVariableSpace = true)>]
    member __.stelem_r4(f) = f +> emit OpCodes.Stelem_R4

    [<CustomOperation("stelem_r8", MaintainsVariableSpace = true)>]
    member __.stelem_r8(f) = f +> emit OpCodes.Stelem_R8

    [<CustomOperation("stelem_ref", MaintainsVariableSpace = true)>]
    member __.stelem_ref(f) = f +> emit OpCodes.Stelem_Ref

    [<CustomOperation("stind_i", MaintainsVariableSpace = true)>]
    member __.stind_i(f) = f +> emit OpCodes.Stind_I

    [<CustomOperation("stind_i1", MaintainsVariableSpace = true)>]
    member __.stind_i1(f) = f +> emit OpCodes.Stind_I1

    [<CustomOperation("stind_i2", MaintainsVariableSpace = true)>]
    member __.stind_i2(f) = f +> emit OpCodes.Stind_I2

    [<CustomOperation("stind_i4", MaintainsVariableSpace = true)>]
    member __.stind_i4(f) = f +> emit OpCodes.Stind_I4

    [<CustomOperation("stind_i8", MaintainsVariableSpace = true)>]
    member __.stind_i8(f) = f +> emit OpCodes.Stind_I8

    [<CustomOperation("stind_r4", MaintainsVariableSpace = true)>]
    member __.stind_r4(f) = f +> emit OpCodes.Stind_R4

    [<CustomOperation("stind_r8", MaintainsVariableSpace = true)>]
    member __.stind_r8(f) = f +> emit OpCodes.Stind_R8

    [<CustomOperation("stind_ref", MaintainsVariableSpace = true)>]
    member __.stind_ref(f) = f +> emit OpCodes.Stind_Ref

    [<CustomOperation("stloc_0", MaintainsVariableSpace = true)>]
    member __.stloc_0(f) = f +> emit OpCodes.Stloc_0

    [<CustomOperation("stloc_1", MaintainsVariableSpace = true)>]
    member __.stloc_1(f) = f +> emit OpCodes.Stloc_1

    [<CustomOperation("stloc_2", MaintainsVariableSpace = true)>]
    member __.stloc_2(f) = f +> emit OpCodes.Stloc_2

    [<CustomOperation("stloc_3", MaintainsVariableSpace = true)>]
    member __.stloc_3(f) = f +> emit OpCodes.Stloc_3

    [<CustomOperation("sub", MaintainsVariableSpace = true)>]
    member __.sub(f) = f +> emit OpCodes.Sub

    [<CustomOperation("sub_ovf", MaintainsVariableSpace = true)>]
    member __.sub_ovf(f) = f +> emit OpCodes.Sub_Ovf

    [<CustomOperation("sub_ovf_un", MaintainsVariableSpace = true)>]
    member __.sub_ovf_un(f) = f +> emit OpCodes.Sub_Ovf_Un

    [<CustomOperation("tailcall", MaintainsVariableSpace = true)>]
    member __.tailcall'(f) = f +> emit OpCodes.Tailcall

    [<CustomOperation("xor", MaintainsVariableSpace = true)>]
    member __.xor(f) = f +> emit OpCodes.Xor
    
    member __.Yield(unit) = fun (u : Universe, il : ILGenerator) -> ()

