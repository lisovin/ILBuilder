﻿namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open ILBuilder
open ILBuilder.Utils

type IL private () =     
    static member add = Emit.EmitOp(OpCodes.Add)
    static member add_ovf = Emit.EmitOp(OpCodes.Add_Ovf)
    static member add_ovf_un = Emit.EmitOp(OpCodes.Add_Ovf_Un)
    static member and' = Emit.EmitOp(OpCodes.And)
    static member arglist = Emit.EmitOp(OpCodes.Arglist)
    static member beginCatchBlock exceptionType = fun (u : Universe, il : ILGenerator) -> il.BeginCatchBlock(u.Import(exceptionType))
    static member beginExceptionBlock(u : Universe, il : ILGenerator) = il.BeginExceptionBlock()
    static member beginFinally(u : Universe, il : ILGenerator) = il.BeginFinallyBlock()
    static member beq label = Emit.EmitLabel(OpCodes.Beq, label)
    static member beq_s label = Emit.EmitLabel(OpCodes.Beq_S, label)
    static member bge label = Emit.EmitLabel(OpCodes.Bge, label)
    static member bge_s label = Emit.EmitLabel(OpCodes.Bge_S, label)
    static member bge_un label = Emit.EmitLabel(OpCodes.Bge_Un, label)
    static member bge_un_s label = Emit.EmitLabel(OpCodes.Bge_Un_S, label)
    static member bgt label = Emit.EmitLabel(OpCodes.Bgt, label)
    static member bgt_s label = Emit.EmitLabel(OpCodes.Bgt_S, label)
    static member bgt_un label = Emit.EmitLabel(OpCodes.Bgt_Un, label)
    static member bgt_un_s label = Emit.EmitLabel(OpCodes.Bgt_Un_S, label)
    static member ble label = Emit.EmitLabel(OpCodes.Ble, label)
    static member ble_s label = Emit.EmitLabel(OpCodes.Ble_S, label)
    static member ble_un label = Emit.EmitLabel(OpCodes.Ble_Un, label)
    static member ble_un_s label = Emit.EmitLabel(OpCodes.Ble_Un_S, label)
    static member blt label = Emit.EmitLabel(OpCodes.Blt, label)
    static member blt_s label = Emit.EmitLabel(OpCodes.Blt_S, label)
    static member blt_un label = Emit.EmitLabel(OpCodes.Blt_Un, label)
    static member blt_un_s label = Emit.EmitLabel(OpCodes.Blt_Un_S, label)
    static member bne_un label = Emit.EmitLabel(OpCodes.Bne_Un, label)
    static member bne_un_s label = Emit.EmitLabel(OpCodes.Bne_Un_S, label)
    static member box (ty : System.Type)  = Emit.EmitType(OpCodes.Box, ty)
    static member box (ty : IKVM.Reflection.Type)  = Emit.EmitType(OpCodes.Box, ty)
    static member br label = Emit.EmitLabel(OpCodes.Br, label)
    static member brfalse label = Emit.EmitLabel(OpCodes.Brfalse, label)
    static member brfalse_s (label : Label)  = Emit.EmitLabel(OpCodes.Brfalse_S, label)
    static member brtrue label = Emit.EmitLabel(OpCodes.Brtrue, label)
    static member brtrue_s label = Emit.EmitLabel(OpCodes.Brtrue_S, label)
    static member br_s label = Emit.EmitLabel(OpCodes.Br_S, label)
    static member call(targetType : Type, methodName, [<ParamArray>] optionalParameterTypes : Type[]) = 
        if targetType = null
        then ArgumentException("targetType") |> raise
        else let mi = targetType.GetMethod(methodName, optionalParameterTypes)
             if mi = null
             then let ps = optionalParameterTypes |> Seq.map (fun t -> t.Name) |> Seq.toString ", "
                  let msg = sprintf "Method '%s' with parameters '%s' is not defined on type '%s'" methodName targetType.Name ps
                  InvalidOperationException(msg) |> raise
             else Emit.EmitMethod(OpCodes.Call, mi)
    static member call (methodInfo : System.Reflection.MethodInfo) = Emit.EmitMethod(OpCodes.Call, methodInfo)
    static member call (methodInfo : MethodInfo) = Emit.EmitMethod(OpCodes.Call, methodInfo)
    static member callvirt (methodInfo : System.Reflection.MethodInfo) = Emit.EmitMethod(OpCodes.Callvirt, methodInfo)
    static member callvirt (methodInfo : MethodInfo) = Emit.EmitMethod(OpCodes.Callvirt, methodInfo)
    static member castclass (ty : Type) = Emit.EmitType(OpCodes.Castclass, ty)
    static member castclass (ty : System.Type) = Emit.EmitType(OpCodes.Castclass, ty)
    static member ceq = Emit.EmitOp(OpCodes.Ceq)
    static member cgt = Emit.EmitOp(OpCodes.Cgt)
    static member cgt_un = Emit.EmitOp(OpCodes.Cgt_Un)
    static member ckfinite = Emit.EmitOp(OpCodes.Ckfinite)
    static member clt = Emit.EmitOp(OpCodes.Clt)
    static member clt_un = Emit.EmitOp(OpCodes.Clt_Un)
    static member constrained (typ : Type) = Emit.EmitType(OpCodes.Constrained, typ)
    static member cpobj (typ : Type) = Emit.EmitType(OpCodes.Cpobj, typ)
    static member declareLocal(t : System.Type) = fun (u : Universe, il : ILGenerator) -> il.DeclareLocal(ofType u t)
    static member declareLocal(tb : TypeBuilder) = fun (u : Universe, il : ILGenerator) -> il.DeclareLocal(tb)
    static member declareLocal<'TLocal>() = fun (u : Universe, il : ILGenerator) -> il.DeclareLocal(ofType u typeof<'TLocal>)
    static member defineLabel (u : Universe, il : ILGenerator) = il.DefineLabel()
    static member endExceptionBlock (u : Universe, il : ILGenerator) = il.EndExceptionBlock()
    static member initobj (typ : Type) = Emit.EmitType(OpCodes.Initobj, typ)
    static member isinst (typ : Type) = Emit.EmitType(OpCodes.Isinst, typ)
    static member jmp (methodInfo : MethodInfo) = Emit.EmitMethod(OpCodes.Jmp, methodInfo)
    static member ldarg_0 = Emit.EmitOp(OpCodes.Ldarg_0)
    static member ldarg_1 = Emit.EmitOp(OpCodes.Ldarg_1)
    static member ldarg_2 = Emit.EmitOp(OpCodes.Ldarg_2)
    static member ldarg_3 = Emit.EmitOp(OpCodes.Ldarg_3)    static member ldarg_s(index: byte) = Emit.EmitByte(OpCodes.Ldarg_S, index)
    static member ldarg_s(index: int16) = Emit.EmitInt16(OpCodes.Ldarg, index)
    static member ldarg(index) =                         
        match index with
        |0 -> IL.ldarg_0
        |1 -> IL.ldarg_1
        |2 -> IL.ldarg_2
        |3 -> IL.ldarg_3
        |n when n <= (int)Byte.MaxValue -> IL.ldarg_s(byte n)
        |n when n <= (int)Int16.MaxValue -> IL.ldarg(n)
        |_ -> ArgumentOutOfRangeException("index") |> raise
    static member ldarg (index : int16) = Emit.EmitInt16(OpCodes.Ldarg, index)
    static member ldarga (index : int16) = Emit.EmitInt16(OpCodes.Ldarga, index)
    static member ldc_bool (b : bool)  = Emit.EmitOp(if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)
    static member ldc_i4 (num : int) = Emit.EmitInt32(OpCodes.Ldc_I4, num)
    static member ldc_i4_s (num : sbyte) = Emit.EmitSByte(OpCodes.Ldc_I4_S, num)
    static member ldc_i4_s (ch : char)  = Emit.EmitSByte(OpCodes.Ldc_I4_S, sbyte ch)
    static member ldc_i8 (num : int64) = Emit.EmitInt64(OpCodes.Ldc_I8, num)
    static member ldc_r4 (num : float32) = Emit.EmitFloat(OpCodes.Ldc_R4, num)
    static member ldc_r8 (num : double) = Emit.EmitDouble(OpCodes.Ldc_R8, num)
    static member ldelema (typ : Type) = Emit.EmitType(OpCodes.Ldelema, typ)
    static member ldfld (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Ldfld, fieldInfo)
    static member ldfld (field : FieldBuilder) = Emit.EmitField(OpCodes.Ldfld, field)
    static member ldflda (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Ldflda, fieldInfo)
    static member ldftn (methodBuilder : MethodBuilder) = Emit.EmitMethod(OpCodes.Ldftn, methodBuilder)
    static member ldftn (methodInfo : System.Reflection.MethodInfo)  = Emit.EmitMethod(OpCodes.Ldftn, methodInfo) 
    static member ldloc (index : int16) = Emit.EmitInt16(OpCodes.Ldloc, index)
    static member ldloc (localBuilder : LocalBuilder) = Emit.EmitLocal(OpCodes.Ldloc, localBuilder)
    static member ldloca (index : int16) = Emit.EmitInt16(OpCodes.Ldloca, index)
    static member ldloca_s (index : byte) = Emit.EmitByte(OpCodes.Ldloca_S, index)
    static member ldloca (local : LocalBuilder) = Emit.EmitLocal(OpCodes.Ldloca, local)
    static member ldloc_s (index : byte) = Emit.EmitByte(OpCodes.Ldloca_S, index)
    static member ldobj (typ : Type) = Emit.EmitType(OpCodes.Ldobj, typ)
    static member ldsfld (fieldBuilder : FieldBuilder) = Emit.EmitField(OpCodes.Ldsfld, fieldBuilder)
    static member ldsfld (fieldInfo : System.Reflection.FieldInfo)  = Emit.EmitField(OpCodes.Ldsfld, fieldInfo)
    static member ldsflda (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Ldsflda, fieldInfo)
    static member ldstr (str : string) = Emit.EmitString(OpCodes.Ldstr, str)
    static member ldtoken (methodInfo : System.Reflection.MethodInfo) = Emit.EmitMethod(OpCodes.Ldtoken, methodInfo)
    static member ldtoken (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Ldtoken, fieldInfo)
    static member ldtoken (ty : Type) = Emit.EmitType(OpCodes.Ldtoken, ty)
    static member ldtoken (ty : System.Type)  = Emit.EmitType(OpCodes.Ldtoken, ty)
    static member ldvirtftn (methodInfo : System.Reflection.MethodInfo) = Emit.EmitMethod(OpCodes.Ldvirtftn, methodInfo)
    static member leave label = Emit.EmitLabel(OpCodes.Leave, label)
    static member leave_s label = Emit.EmitLabel(OpCodes.Leave_S, label)
    static member markLabel (label : Label) = fun (u : Universe, il : ILGenerator) -> il.MarkLabel(label)
    static member mkrefany (elementType : System.Type)  = Emit.EmitType(OpCodes.Mkrefany, elementType)
    static member newarr (elementType : System.Type)  = Emit.EmitType(OpCodes.Newarr, elementType) 
    static member newarr (elementType : IKVM.Reflection.Type)  = Emit.EmitType(OpCodes.Newarr, elementType) 
    static member newobj (cons : System.Reflection.ConstructorInfo) = Emit.EmitConstructor(OpCodes.Newobj, cons)
    static member newobj<'t when 't : (new : unit -> 't)>() = 
        let ci = typeof<'t>.GetConstructor([||])
        Emit.EmitConstructor(OpCodes.Newobj, ci)
    static member newobj(t : Type) = 
        let ci = t.GetConstructor([||])
        Emit.EmitConstructor(OpCodes.Newobj, ci)
    static member newobj(t : System.Type) = Emit.EmitConstructor(OpCodes.Newobj, t.GetConstructor([||]))
    static member newobj(cb : ConstructorInfo) = Emit.EmitConstructor(OpCodes.Newobj, cb) 
    static member newobj(cb : ConstructorBuilder) = Emit.EmitConstructor(OpCodes.Newobj, cb) 
    static member refanyval (typ : Type) = Emit.EmitType(OpCodes.Refanyval, typ)
    static member ret = Emit.EmitOp(OpCodes.Ret)
    static member sizeof (typ : Type) = Emit.EmitType(OpCodes.Sizeof, typ)
    static member starg (index : int16) = Emit.EmitInt16(OpCodes.Starg, index)
    static member starg_s (index : byte) = Emit.EmitByte(OpCodes.Starg_S, index)
    static member stfld (fieldBuilder : FieldBuilder) = Emit.EmitField(OpCodes.Stfld, fieldBuilder)
    static member stfld (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Stfld, fieldInfo)
    static member stloc (local : LocalBuilder) = Emit.EmitLocal(OpCodes.Stloc, local)
    static member stloc_s (local : LocalBuilder) = Emit.EmitLocal(OpCodes.Stloc_S, local)
    static member stobj (typ : Type) = Emit.EmitType(OpCodes.Stobj, typ)
    static member stsfld (fieldInfo : System.Reflection.FieldInfo) = Emit.EmitField(OpCodes.Stsfld, fieldInfo)
    static member unaligned label = Emit.EmitLabel(OpCodes.Unaligned, label)
    static member unaligned (addr : int64) = Emit.EmitInt64(OpCodes.Unaligned, addr)
    static member unbox (typ : Type) = Emit.EmitType(OpCodes.Unbox, typ)
    static member unbox (typ : System.Type) = Emit.EmitType(OpCodes.Unbox, typ)
    static member unbox_any (ty : Type)  = Emit.EmitType(OpCodes.Unbox_Any, ty)
    static member unbox_any (ty : System.Type)  = Emit.EmitType(OpCodes.Unbox_Any, ty)
    static member conv_i = Emit.EmitOp(OpCodes.Conv_I)
    static member conv_i1 = Emit.EmitOp(OpCodes.Conv_I1)
    static member conv_i2 = Emit.EmitOp(OpCodes.Conv_I2)
    static member conv_i4 = Emit.EmitOp(OpCodes.Conv_I4)
    static member conv_i8 = Emit.EmitOp(OpCodes.Conv_I8)
    static member conv_ovf_i = Emit.EmitOp(OpCodes.Conv_Ovf_I)
    static member conv_ovf_i1 = Emit.EmitOp(OpCodes.Conv_Ovf_I1)
    static member conv_ovf_i1_un = Emit.EmitOp(OpCodes.Conv_Ovf_I1_Un)
    static member conv_ovf_i2 = Emit.EmitOp(OpCodes.Conv_Ovf_I2)
    static member conv_ovf_i2_un = Emit.EmitOp(OpCodes.Conv_Ovf_I2_Un)
    static member conv_ovf_i4 = Emit.EmitOp(OpCodes.Conv_Ovf_I2_Un)
    static member conv_ovf_i4_un = Emit.EmitOp(OpCodes.Conv_Ovf_I4_Un)
    static member conv_ovf_i8 = Emit.EmitOp(OpCodes.Conv_Ovf_I8)
    static member conv_ovf_i8_un = Emit.EmitOp(OpCodes.Conv_Ovf_I8_Un)
    static member conv_ovf_i_un = Emit.EmitOp(OpCodes.Conv_Ovf_I_Un)
    static member conv_ovf_u = Emit.EmitOp(OpCodes.Conv_Ovf_U)
    static member conv_ovf_u1 = Emit.EmitOp(OpCodes.Conv_Ovf_U1)
    static member conv_ovf_u1_un = Emit.EmitOp(OpCodes.Conv_Ovf_U1_Un)
    static member conv_ovf_u2 = Emit.EmitOp(OpCodes.Conv_Ovf_U2)
    static member conv_ovf_u2_un = Emit.EmitOp(OpCodes.Conv_Ovf_U2_Un)
    static member conv_ovf_u4 = Emit.EmitOp(OpCodes.Conv_Ovf_U4)
    static member conv_ovf_u4_un = Emit.EmitOp(OpCodes.Conv_Ovf_U4_Un)
    static member conv_ovf_u8 = Emit.EmitOp(OpCodes.Conv_Ovf_U8)
    static member conv_ovf_u8_un = Emit.EmitOp(OpCodes.Conv_Ovf_U8_Un)
    static member conv_ovf_u_un = Emit.EmitOp(OpCodes.Conv_Ovf_U_Un)
    static member conv_r4 = Emit.EmitOp(OpCodes.Conv_R4)
    static member conv_r8 = Emit.EmitOp(OpCodes.Conv_R8)
    static member conv_r_un = Emit.EmitOp(OpCodes.Conv_R_Un)
    static member conv_u = Emit.EmitOp(OpCodes.Conv_U)
    static member conv_u1 = Emit.EmitOp(OpCodes.Conv_U1)
    static member conv_u2 = Emit.EmitOp(OpCodes.Conv_U2)
    static member conv_u4 = Emit.EmitOp(OpCodes.Conv_U4)
    static member conv_u8 = Emit.EmitOp(OpCodes.Conv_U8)
    static member cpblk = Emit.EmitOp(OpCodes.Cpblk)
    static member div = Emit.EmitOp(OpCodes.Div)
    static member div_un = Emit.EmitOp(OpCodes.Div_Un)
    static member dup = Emit.EmitOp(OpCodes.Dup)
    static member endfilter = Emit.EmitOp(OpCodes.Endfilter)
    static member endfinally = Emit.EmitOp(OpCodes.Endfinally)
    static member initblk = Emit.EmitOp(OpCodes.Initblk)
    static member ldc_i4_0 = Emit.EmitOp(OpCodes.Ldc_I4_0)
    static member ldc_i4_1 = Emit.EmitOp(OpCodes.Ldc_I4_1)
    static member ldc_i4_2 = Emit.EmitOp(OpCodes.Ldc_I4_2)
    static member ldc_i4_3 = Emit.EmitOp(OpCodes.Ldc_I4_3)
    static member ldc_i4_4 = Emit.EmitOp(OpCodes.Ldc_I4_4)
    static member ldc_i4_5 = Emit.EmitOp(OpCodes.Ldc_I4_5)
    static member ldc_i4_6 = Emit.EmitOp(OpCodes.Ldc_I4_6)
    static member ldc_i4_7 = Emit.EmitOp(OpCodes.Ldc_I4_7)
    static member ldc_i4_8 = Emit.EmitOp(OpCodes.Ldc_I4_8)
    static member ldc_i4_m1 = Emit.EmitOp(OpCodes.Ldc_I4_M1)
    static member ldelem_i = Emit.EmitOp(OpCodes.Ldelem_I)
    static member ldelem_i1 = Emit.EmitOp(OpCodes.Ldelem_I1)
    static member ldelem_i2 = Emit.EmitOp(OpCodes.Ldelem_I2)
    static member ldelem_i4 = Emit.EmitOp(OpCodes.Ldelem_I4)
    static member ldelem_i8 = Emit.EmitOp(OpCodes.Ldelem_I8)
    static member ldelem_r4 = Emit.EmitOp(OpCodes.Ldelem_R4)
    static member ldelem_r8 = Emit.EmitOp(OpCodes.Ldelem_R8)
    static member ldelem_ref = Emit.EmitOp(OpCodes.Ldelem_Ref)
    static member ldelem_u1 = Emit.EmitOp(OpCodes.Ldelem_U1)
    static member ldelem_u2 = Emit.EmitOp(OpCodes.Ldelem_U2)
    static member ldelem_u4 = Emit.EmitOp(OpCodes.Ldelem_U4)
    static member ldind_i = Emit.EmitOp(OpCodes.Ldind_I)
    static member ldind_i1 = Emit.EmitOp(OpCodes.Ldind_I1)
    static member ldind_i2 = Emit.EmitOp(OpCodes.Ldind_I2)
    static member ldind_i4 = Emit.EmitOp(OpCodes.Ldind_I4)
    static member ldind_i8 = Emit.EmitOp(OpCodes.Ldind_I8)
    static member ldind_r4 = Emit.EmitOp(OpCodes.Ldind_R4)
    static member ldind_r8 = Emit.EmitOp(OpCodes.Ldind_R8)
    static member ldind_ref = Emit.EmitOp(OpCodes.Ldind_Ref)
    static member ldind_u1 = Emit.EmitOp(OpCodes.Ldind_U1)
    static member ldind_u2 = Emit.EmitOp(OpCodes.Ldind_U2)
    static member ldind_u4 = Emit.EmitOp(OpCodes.Ldind_U4)
    static member ldlen = Emit.EmitOp(OpCodes.Ldlen)
    static member ldloc_0 = Emit.EmitOp(OpCodes.Ldloc_0)
    static member ldloc_1 = Emit.EmitOp(OpCodes.Ldloc_1)
    static member ldloc_2 = Emit.EmitOp(OpCodes.Ldloc_2)
    static member ldloc_3 = Emit.EmitOp(OpCodes.Ldloc_3)
    static member ldnull = Emit.EmitOp(OpCodes.Ldnull)
    static member localloc = Emit.EmitOp(OpCodes.Localloc)
    static member mul = Emit.EmitOp(OpCodes.Mul)
    static member mul_ovf = Emit.EmitOp(OpCodes.Mul_Ovf)
    static member mul_ovf_un = Emit.EmitOp(OpCodes.Mul_Ovf_Un)
    static member neg = Emit.EmitOp(OpCodes.Neg)
    static member nop = Emit.EmitOp(OpCodes.Nop)
    static member not' = Emit.EmitOp(OpCodes.Not)
    static member or' = Emit.EmitOp(OpCodes.Or)
    static member pop = Emit.EmitOp(OpCodes.Pop)
    static member refanytype = Emit.EmitOp(OpCodes.Refanytype)
    static member rem = Emit.EmitOp(OpCodes.Rem)
    static member rem_un = Emit.EmitOp(OpCodes.Rem_Un)
    static member rethrow = Emit.EmitOp(OpCodes.Rethrow)
    static member shl = Emit.EmitOp(OpCodes.Shl)
    static member shr = Emit.EmitOp(OpCodes.Shr)
    static member shr_un = Emit.EmitOp(OpCodes.Shr_Un)
    static member stelem (ty : Type) = Emit.EmitType(OpCodes.Stelem, ty)
    static member stelem<'TElement>() = Emit.EmitType(OpCodes.Stelem, typeof<'TElement>)
    static member stelem (ty : System.Type) = Emit.EmitType(OpCodes.Stelem, ty)
    static member stelem (ty : TypeBuilder) = Emit.EmitType(OpCodes.Stelem, ty)
    static member stelem_i = Emit.EmitOp(OpCodes.Stelem_I)
    static member stelem_i1 = Emit.EmitOp(OpCodes.Stelem_I1)
    static member stelem_i2 = Emit.EmitOp(OpCodes.Stelem_I2)
    static member stelem_i4 = Emit.EmitOp(OpCodes.Stelem_I4)
    static member stelem_i8 = Emit.EmitOp(OpCodes.Stelem_I8)
    static member stelem_r4 = Emit.EmitOp(OpCodes.Stelem_R4)
    static member stelem_r8 = Emit.EmitOp(OpCodes.Stelem_R8)
    static member stelem_ref = Emit.EmitOp(OpCodes.Stelem_Ref)
    static member stind_i = Emit.EmitOp(OpCodes.Stind_I)
    static member stind_i1 = Emit.EmitOp(OpCodes.Stind_I1)
    static member stind_i2 = Emit.EmitOp(OpCodes.Stind_I2)
    static member stind_i4 = Emit.EmitOp(OpCodes.Stind_I4)
    static member stind_i8 = Emit.EmitOp(OpCodes.Stind_I8)
    static member stind_r4 = Emit.EmitOp(OpCodes.Stind_R4)
    static member stind_r8 = Emit.EmitOp(OpCodes.Stind_R8)
    static member stind_ref = Emit.EmitOp(OpCodes.Stind_Ref)
    static member stloc_0 = Emit.EmitOp(OpCodes.Stloc_0)
    static member stloc_1 = Emit.EmitOp(OpCodes.Stloc_1)
    static member stloc_2 = Emit.EmitOp(OpCodes.Stloc_2)
    static member stloc_3 = Emit.EmitOp(OpCodes.Stloc_3)
    static member sub = Emit.EmitOp(OpCodes.Sub)
    static member sub_ovf = Emit.EmitOp(OpCodes.Sub_Ovf)
    static member sub_ovf_un = Emit.EmitOp(OpCodes.Sub_Ovf_Un)
    static member tailcall' = Emit.EmitOp(OpCodes.Tailcall)
    static member xor = Emit.EmitOp(OpCodes.Xor)