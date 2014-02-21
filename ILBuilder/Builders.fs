namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open ILBuilder
open ILBuilder.Utils

type LdtokenArg = 
| Field of System.Reflection.FieldInfo
| Method of System.Reflection.MethodInfo
| Type of System.Type

type NewobjArg = 
| ClrConstructor of System.Reflection.ConstructorInfo
| IkvmConstructor of ConstructorInfo
//| ConstructorBuilder of Emit.ConstructorBuilder
| Type of System.Type

type FieldArg = 
| Field of System.Reflection.FieldInfo
| FieldBuilder of Emit.FieldBuilder

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
    member __.beginCatchBlock(f, exceptionType) = fun (u : Universe, il : ILGenerator) -> il.BeginCatchBlock(u.Import(exceptionType))

    [<CustomOperation("beginExceptionBlock", MaintainsVariableSpace = true)>]
    member __.beginExceptionBlock(f) = fun (il : ILGenerator) -> il.BeginExceptionBlock()

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
    member __.box (f, typ : System.Type) = fun (u : Universe, il : ILGenerator) -> il.Emit(OpCodes.Box, u.Import(typ)) 

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
    (*
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
    *)
    [<CustomOperation("call", MaintainsVariableSpace = true)>]
    member __.call (f, methodInfo : System.Reflection.MethodInfo)=     
        if methodInfo = null then ArgumentNullException("methodInfo", "Method info cannot be null.") |> raise
        f +> emitMethodInfo OpCodes.Call methodInfo

    [<CustomOperation("callvirt", MaintainsVariableSpace = true)>]
    member __.callvirt (f, methodInfo : System.Reflection.MethodInfo) = 
        f +> emitMethodInfo OpCodes.Callvirt methodInfo

    [<CustomOperation("castclass", MaintainsVariableSpace = true)>]
    member __.castclass (f, ty : System.Type) = 
        f +> emitType OpCodes.Castclass ty

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
    member __.constrained (f, ty) = f +> emitType OpCodes.Constrained ty

    [<CustomOperation("cpobj", MaintainsVariableSpace = true)>]
    member __.cpobj (f, ty) = f +> emitType OpCodes.Cpobj ty

    [<CustomOperation("declareLocal", MaintainsVariableSpace = true)>]
    member __.declareLocal(f, ty) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            il.DeclareLocal(ofType u ty)

    [<CustomOperation("defineLabel", MaintainsVariableSpace = true)>]
    member __.defineLabel(f) = 
        fun (u : Universe, il : ILGenerator) ->
            f(u, il)
            il.DefineLabel()

    [<CustomOperation("endExceptionBlock", MaintainsVariableSpace = true)>]
    member __.endExceptionBlock(f) = fun (il : ILGenerator) -> il.EndExceptionBlock()

    [<CustomOperation("initobj", MaintainsVariableSpace = true)>]
    member __.initobj (f, ty) = f +> emitType OpCodes.Initobj ty 

    [<CustomOperation("isinst", MaintainsVariableSpace = true)>]
    member __.isinst (f, ty) = f +> emitType OpCodes.Isinst ty 

    [<CustomOperation("jmp", MaintainsVariableSpace = true)>]
    member __.jmp (f, methodInfo) = f +> emitMethodInfo OpCodes.Jmp methodInfo 

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
    member __.ldc_bool (f, b : bool) = f +> emit (if b then OpCodes.Ldc_I4_1 else OpCodes.Ldc_I4_0)

    [<CustomOperation("ldc_i4", MaintainsVariableSpace = true)>]
    member __.ldc_i4(f, num : int) = f +> emitInt32 OpCodes.Ldc_I4 num 
    (*
    [<CustomOperation("ldc_i4_s", MaintainsVariableSpace = true)>]
    member __.ldc_i4_s(f, num : sbyte) = f +> emitSByte OpCodes.Ldc_I4_S num 
    *)
    [<CustomOperation("ldc_i4_s", MaintainsVariableSpace = true)>]
    member __.ldc_i4_s (f, ch : char) = f +> emitSByte OpCodes.Ldc_I4_S (sbyte ch)

    [<CustomOperation("ldc_i8", MaintainsVariableSpace = true)>]
    member __.ldc_i8(f, num : int64) = f +> emitInt64 OpCodes.Ldc_I8 num 

    [<CustomOperation("ldc_r4", MaintainsVariableSpace = true)>]
    member __.ldc_r4 (f, num : float32) = f +> emitFloat OpCodes.Ldc_R4 num 

    [<CustomOperation("ldc_r8", MaintainsVariableSpace = true)>]
    member __.ldc_r8 (f, num : double) = f +> emitDouble OpCodes.Ldc_R8 num 

    [<CustomOperation("ldelema", MaintainsVariableSpace = true)>]
    member __.ldelema (f, ty) = f +> emitType OpCodes.Ldelema ty 

    [<CustomOperation("ldfld", MaintainsVariableSpace = true)>]
    member __.ldfld (f, field : FieldArg) = 
        fun (u : Universe, il : ILGenerator) ->
            f(u, il)
            match field with
            | FieldArg.Field fi -> il.Emit(OpCodes.Ldfld, ofFieldInfo u fi)
            | FieldArg.FieldBuilder fb -> il.Emit(OpCodes.Ldfld, fb)

    [<CustomOperation("ldflda", MaintainsVariableSpace = true)>]
    member __.ldflda (f, fieldInfo) = f +> emitFieldInfo OpCodes.Ldflda fieldInfo 

    [<CustomOperation("ldftn", MaintainsVariableSpace = true)>]
    member __.ldftn (f, methodBuilder : MethodBuilder) = f +> emitMethodBuilder OpCodes.Ldftn methodBuilder 

    [<CustomOperation("ldftn", MaintainsVariableSpace = true)>]
    member __.ldftn (f, methodInfo) = f +> emitMethodInfo methodInfo

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
    member __.ldobj (f, ty) = f +> emitType OpCodes.Ldobj ty

    [<CustomOperation("ldsfld", MaintainsVariableSpace = true)>]
    member __.ldsfld (f, field : FieldArg) = 
        fun (u : Universe, il : ILGenerator) ->
            f(u, il)
            match field with
            | FieldArg.Field fi -> il.Emit(OpCodes.Ldsfld, ofFieldInfo u fi)
            | FieldArg.FieldBuilder fb -> il.Emit(OpCodes.Ldsfld, fb)

    [<CustomOperation("ldsflda", MaintainsVariableSpace = true)>]
    member __.ldsflda (f, fieldInfo) = f +> emitFieldInfo OpCodes.Ldsflda fieldInfo 

    [<CustomOperation("ldstr", MaintainsVariableSpace = true)>]
    member __.ldstr (f, str : string)= f +> emitString OpCodes.Ldstr str 

    [<CustomOperation("ldtoken", MaintainsVariableSpace = true)>]
    member __.ldtoken (f, token : LdtokenArg) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            match token with 
            | Method methodInfo -> il.Emit(OpCodes.Ldtoken, ofMethodInfo u methodInfo)
            | LdtokenArg.Field fieldInfo -> il.Emit(OpCodes.Ldtoken, ofFieldInfo u fieldInfo)
            | LdtokenArg.Type ty -> il.Emit(OpCodes.Ldtoken, ofType u ty)

    [<CustomOperation("ldvirtftn", MaintainsVariableSpace = true)>]
    member __.ldvirtftn (f, methodInfo) = f +> emitMethodInfo OpCodes.Ldvirtftn 

    [<CustomOperation("leave", MaintainsVariableSpace = true)>]
    member __.leave(f, label) = f +> emitLabel OpCodes.Leave label 

    [<CustomOperation("leave_s", MaintainsVariableSpace = true)>]
    member __.leave_s(label, f) = f +> emitLabel OpCodes.Leave_S label 

    [<CustomOperation("markLabel", MaintainsVariableSpace = true)>]
    member __.markLabel(f, label : Label) = f +> fun (_, il : ILGenerator) -> il.MarkLabel(label)

    [<CustomOperation("mkrefany", MaintainsVariableSpace = true)>]
    member __.mkrefany (f, ty) = f +> emitType OpCodes.Mkrefany ty

    [<CustomOperation("newarr", MaintainsVariableSpace = true)>]
    member __.newarr (f, ty) = f +> emitType OpCodes.Newarr ty

    [<CustomOperation("newobj", MaintainsVariableSpace = true)>]
    member __.newobj (f, arg : NewobjArg) = 
        fun (u : Universe, il : ILGenerator) -> 
            f(u, il)
            match arg with
            | NewobjArg.ClrConstructor ci ->
                il.Emit(OpCodes.Newobj, ofConstructorInfo u ci)
            | NewobjArg.IkvmConstructor ci ->
                il.Emit(OpCodes.Newobj, ci)
            | NewobjArg.Type ty ->
                let ci = (ofType u ty).GetConstructor([||])         
                il.Emit(OpCodes.Newobj, ci)

    [<CustomOperation("refanyval", MaintainsVariableSpace = true)>]
    member __.refanyval (f, ty) = f +> emitType OpCodes.Refanyval ty

    [<CustomOperation("ret", MaintainsVariableSpace = true)>]
    member __.ret(f) = f +> emit OpCodes.Ret 

    [<CustomOperation("sizeof", MaintainsVariableSpace = true)>]
    member __.sizeof (f, ty) = f +> emitType OpCodes.Sizeof ty

    [<CustomOperation("starg", MaintainsVariableSpace = true)>]
    member __.starg (f, index : int16) = f +> emitInt16 OpCodes.Starg index 

    [<CustomOperation("starg_s", MaintainsVariableSpace = true)>]
    member __.starg_s (f, index : byte) = f +> emitByte OpCodes.Starg_S index 

    [<CustomOperation("stfld", MaintainsVariableSpace = true)>]
    member __.stfld (f, field : FieldArg) = 
        fun (u : Universe, il : ILGenerator) ->
            f(u, il)
            match field with
            | Field fi -> il.Emit(OpCodes.Stfld, ofFieldInfo u fi)
            | FieldBuilder fb -> il.Emit(OpCodes.Stfld, fb)

    [<CustomOperation("stloc", MaintainsVariableSpace = true)>]
    member __.stloc (f, local : LocalBuilder) = f +> emitLocalBuilder OpCodes.Stloc local 

    [<CustomOperation("stloc_s", MaintainsVariableSpace = true)>]
    member __.stloc_s (f, local : LocalBuilder)= f +> emitLocalBuilder OpCodes.Stloc_S local 

    [<CustomOperation("stobj", MaintainsVariableSpace = true)>]
    member __.stobj (f, ty)= f +> emitType OpCodes.Stobj ty

    [<CustomOperation("stsfld", MaintainsVariableSpace = true)>]
    member __.stsfld (f, fieldInfo) = f +> emitFieldInfo OpCodes.Stsfld fieldInfo 

    [<CustomOperation("thisType", MaintainsVariableSpace = true)>]
    member __.thisType(f) = fun (il : ILGenerator, mb : MethodBuilder) -> mb.DeclaringType

    [<CustomOperation("unaligned", MaintainsVariableSpace = true)>]
    member __.unaligned(f, label) = f +> emitLabel OpCodes.Unaligned label 

    [<CustomOperation("unaligned", MaintainsVariableSpace = true)>]
    member __.unaligned (f, addr : int64) = f +> emitInt64 OpCodes.Unaligned addr 

    [<CustomOperation("unbox", MaintainsVariableSpace = true)>]
    member __.unbox (f, ty) = f +> emitType OpCodes.Unbox ty

    [<CustomOperation("unbox_any", MaintainsVariableSpace = true)>]
    member __.unbox_any (f, typ : System.Type) = fun (u : Universe, il : ILGenerator) -> il.Emit(OpCodes.Unbox_Any, u.Import(typ)) 

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

    [<CustomOperation("stelem", MaintainsVariableSpace = true)>]
    member __.stelem(f, ty) = f +> emitType OpCodes.Stelem ty

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

type BuilderType =
| ThisType
| ClrType of System.Type
| IkvmType of Type
    static member ToIkvmType (u : Universe) (declaringType : Type) x = 
        match x with
        | ThisType -> declaringType
        | ClrType ty -> ty |> Utils.ofType u
        | IkvmType ty -> ty


type IKVMMethodBuilder(name : string, atts, returnType : BuilderType, parameterTypes, ?export : string * int) = 
    inherit EmitBuilder()

    member __.Run(f : Universe * ILGenerator -> unit) = 
        fun (u : Universe, tb : TypeBuilder) ->
            let methodBuilder = tb.DefineMethod(name, atts)
            methodBuilder.SetReturnType(returnType |> BuilderType.ToIkvmType u methodBuilder.DeclaringType)
            methodBuilder.SetParameters(parameterTypes |> Array.map (BuilderType.ToIkvmType u methodBuilder.DeclaringType))
            let il = methodBuilder.GetILGenerator()
            f (u, il)
            match export with
            |Some (name, n) -> methodBuilder.__AddUnmanagedExport(name, n)
            |_ -> ()
            methodBuilder

type PropertyAccessor = Get | Set

type IKVMPropertyAccessorBuilder(accessor) = 
    inherit EmitBuilder()

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
    let mutable propField = null

    [<CustomOperation("get")>]
    member __.Getter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("get_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, returnType, System.Type.EmptyTypes |> Array.map ClrType)
            let getter = il {
                ldarg_0
                ldfld (FieldBuilder propField)
                ret
            }
            pb.SetGetMethod(getter(u, tb))

    [<CustomOperation("set")>]
    member __.Setter(f) = 
        fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> 
            f(u, tb, pb)
            let il = IKVMMethodBuilder("set_" + name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, ClrType typeof<System.Void>, [|returnType|])
            let setter = il {
                ldarg_0
                ldarg_1
                stfld (FieldBuilder propField)
                ret
            }
            pb.SetSetMethod(setter(u, tb))

    member __.Yield(unit) = fun (u : Universe, tb : TypeBuilder, pb : PropertyBuilder) -> ()

    member __.Run(f) = 
        fun (u : Universe, tb : TypeBuilder) -> 
            let rty = returnType |> BuilderType.ToIkvmType u tb.DeclaringType
            let pb = tb.DefineProperty(name, PropertyAttributes.None, rty, parameterTypes |> Array.map (BuilderType.ToIkvmType u tb.DeclaringType))
            propField <- tb.DefineField(name, rty, FieldAttributes.Private)
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
            
type IKVMTypeBuilder(name, atts) = 
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
            let bound : Type = define(u, mb)
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

    (*
     * Types
     *)
    let publicType typeName = IKVMTypeBuilder(typeName, TypeAttributes.Public)

    let internalType typeName = IKVMTypeBuilder(typeName, TypeAttributes.NotPublic)

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
    (*
        IKVMMethodBuilder("get_" + pb.Name, MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig, pb.PropertyType |> Utils.toSystemType, System.Type.EmptyTypes)
        *)
            (*let getter = il {
                ldarg_0
                ret
            }
            pb.SetGetMethod(getter(u, tb))*)
        (*
    let publicPropertyBackedByField<'t> propName fieldBuilder = 
        publicPropertyBackedByFieldOfType typeof<'t> propName
    *)
    (*
     * Save
     *)
    let saveAssembly assemblyPath (f : Universe * string -> AssemblyBuilder) = 
        use universe = new Universe()
        let ab = f (universe, assemblyPath)
        let name = ab.GetName().Name + ".dll"
        ab.Save(name)

(*  
module TestIL = 
    type Parent() = 
        member p.Bar() = ()
        type Child() = 
            member c.Foo() = ()
*)

        (*
module F = 
    assembly "" {
        do! publicType "" {
            do! publicProperty<string> "" {
                do! get {
                    ret
                }
                
                //get {
                //    ret 
                //}
            }
        }
    } |> ignore
   *) 