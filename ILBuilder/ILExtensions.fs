namespace ILBuilder

open System
open System.IO

open IKVM.Reflection
open IKVM.Reflection.Emit

open ILBuilder
open ILBuilder.Utils

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

[<AutoOpen>]
module ILExtensions = 
    type ILBuilder.IL with 
        (*
         *
         *)
        static member emitArray<'TElement>(count, initializer : int -> 'TElement) =
            let ldelem, stelem = 
                match typeof<'TElement> with
                | ty when ty = typeof<bool> -> 
                    (fun i -> IL.ldc_bool (initializer i |> box |> unbox)),
                    IL.stelem_i
                | ty when ty = typeof<int32> -> 
                    (fun i -> IL.ldc_i4 (initializer i |> box |> unbox)),
                    IL.stelem_i
                | ty when ty = typeof<int64> -> 
                    (fun i -> IL.ldc_i8 (initializer i |> box |> unbox)),
                    IL.stelem_i
                | ty when ty = typeof<string> -> 
                    (fun i -> IL.ldstr (initializer i |> box |> unbox)),
                    IL.stelem_ref
                | ty -> 
                    failwithf "Not supported type of element '%s'" ty.FullName

            il {
                let! loc = IL.declareLocal<'TElement[]>()
                do! IL.ldc_i4 count
                do! IL.newarr typeof<'TElement>
                do! IL.stloc loc
                for i in 0..count-1 do
                    do! IL.ldloc loc
                    do! IL.ldc_i4 i
                    do! ldelem i
                    do! stelem
                do! IL.ldloc loc    
            }
    
        static member emitArray (xs : #seq<'TElement>) = 
            let arr = xs |> Seq.toArray
            IL.emitArray(arr.Length, fun i -> arr.[i])

        static member emitArray<'TElement when 'TElement : not struct> (initializers : seq<Universe * ILGenerator -> unit>) = 
            let instrs = initializers |> Seq.toArray
            let count = instrs.Length
            il {
                do! IL.ldc_i4 count
                do! IL.newarr typeof<'TElement>
                for i in 0..count-1 do
                    do! IL.dup
                    do! IL.ldc_i4 i
                    do! instrs.[i]
                    do! IL.stelem typeof<'TElement>
            }
        
        static member emitArrayOfValues<'TElement> (props : seq<PropertyBuilder>) = 
            let props = props |> Seq.toArray
            let count = props.Length
            let propType = typeof<'TElement>
            il {
                do! IL.ldc_i4 count
                do! IL.newarr propType
                for i in 0..count-1 do
                    do! IL.dup
                    do! IL.ldc_i4 i
                    do! IL.ldarg_0
                    let prop = props.[i]
                    do! IL.callvirt (prop.GetGetMethod())
                    do! IL.box prop.PropertyType
                    //do! IL.castclass propType
                    do! IL.stelem propType
            }


        static member ilprintf format = 
            let il = IKVMILBuilder()
            Printf.ksprintf (fun s -> il {
                do! IL.ldstr s
                do! IL.call (typeof<Console>.GetMethod("WriteLine", [| typeof<string> |]))
            }) format

        static member ifThen condition thenBody = 
            il {
                let! notTrue = IL.defineLabel
                do! condition notTrue
                do! thenBody
                do! IL.br_s notTrue
                do! IL.markLabel notTrue
            }

        static member ifThenElse condition thenBody elseBody =    
            il {
                let! notTrue = IL.defineLabel
                let! endIf = IL.defineLabel
                do! condition notTrue
                do! thenBody
                do! IL.br_s endIf
                do! IL.markLabel notTrue
                do! elseBody
                do! IL.markLabel endIf
            }
    
        (*
         * This
         *)
        static member thisType(u : Universe, mb : MethodBuilder) = mb.DeclaringType

        static member thisType(u : Universe, tb : TypeBuilder) = tb.MakeByRefType().GetElementType()
            
        (*
         * Types
         *)
        static member publicType typeName = IKVMTypeBuilder(typeName, TypeAttributes.Public)

        static member internalType typeName = IKVMTypeBuilder(typeName, TypeAttributes.NotPublic)

        static member extensionsType typeName = 
            let ci = typeof<System.Runtime.CompilerServices.ExtensionAttribute>.GetConstructor([||])
            IKVMTypeBuilder(typeName, TypeAttributes.Public ||| TypeAttributes.Sealed, ci)

        static member nestedPublicType typeName = IKVMNestedTypeBuilder(typeName, TypeAttributes.NestedPublic)

        (*
         * Constructors
         *)
        static member publicConstructor ([<ParamArray>] parameterTypes : System.Type[]) = 
            IKVMConstructorBuilder(MethodAttributes.Public, parameterTypes |> Array.map ClrType)

        static member publicConstructor ([<ParamArray>] parameterTypes : Type[]) = 
            IKVMConstructorBuilder(MethodAttributes.Public, parameterTypes |> Array.map GenType)

        static member publicConstructor = 
            IKVMConstructorBuilder(MethodAttributes.Public, [||])

        static member publicDefaultConstructor (u : Universe, tb : TypeBuilder) =
            tb.DefineDefaultConstructor(MethodAttributes.Public)

        (*
         * Methods
         *)
        // public
        static member defineMethod(name, atts, returnType, parameterTypes) = 
            let unitToVoid = function 
                             | ClrType ty when ty = typeof<unit> -> ClrType typeof<System.Void>
                             | t -> t
            let returnType = unitToVoid returnType
            let parameterTypes = parameterTypes |> Array.map unitToVoid

            IKVMMethodBuilder(name, atts, returnType, parameterTypes)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.defineMethod(name, MethodAttributes.Public, returnType, parameterTypes)
            
        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(ClrType returnType, name, parameterTypes)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(GenType returnType, name, parameterTypes)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(ClrType returnType, name, parameterTypes |> Array.map ClrType)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(GenType returnType, name, parameterTypes |> Array.map ClrType)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(ClrType returnType, name, parameterTypes |> Array.map GenType)

        static member publicMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(GenType returnType, name, parameterTypes |> Array.map GenType)

        static member publicMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(typeof<'TReturn>, name, parameterTypes)

        static member publicMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(typeof<'TReturn>, name, parameterTypes |> Array.map ClrType)

        static member publicMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicMethod(typeof<'TReturn>, name, parameterTypes |> Array.map GenType)

        static member publicMethod<'TReturn>(name) = 
            IL.publicMethod(typeof<'TReturn>, name, [||])

        static member publicMethod(returnType, name) = 
            IL.publicMethod(GenType returnType, name, [||])

        // private
        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.defineMethod(name, MethodAttributes.Private, returnType, parameterTypes)
            
        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(ClrType returnType, name, parameterTypes)

        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(GenType returnType, name, parameterTypes)

        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(ClrType returnType, name, parameterTypes |> Array.map ClrType)

        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(GenType returnType, name, parameterTypes |> Array.map ClrType)

        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(ClrType returnType, name, parameterTypes |> Array.map GenType)

        static member privateMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(GenType returnType, name, parameterTypes |> Array.map GenType)

        static member privateMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(typeof<'TReturn>, name, parameterTypes)

        static member privateMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(typeof<'TReturn>, name, parameterTypes |> Array.map ClrType)

        static member privateMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateMethod(typeof<'TReturn>, name, parameterTypes |> Array.map GenType)

        static member privateMethod<'TReturn>(name) = 
            IL.privateMethod(typeof<'TReturn>, name, [||])

        static member privateMethod(returnType, name) = 
            IL.privateMethod(GenType returnType, name, [||])

        // private static
        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.defineMethod(name, MethodAttributes.Private ||| MethodAttributes.Static, returnType, parameterTypes)
            
        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(ClrType returnType, name, parameterTypes)

        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(GenType returnType, name, parameterTypes)

        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(ClrType returnType, name, parameterTypes |> Array.map ClrType)

        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(GenType returnType, name, parameterTypes |> Array.map ClrType)

        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(ClrType returnType, name, parameterTypes |> Array.map GenType)

        static member privateStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(GenType returnType, name, parameterTypes |> Array.map GenType)

        static member privateStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(typeof<'TReturn>, name, parameterTypes)

        static member privateStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(typeof<'TReturn>, name, parameterTypes |> Array.map ClrType)

        static member privateStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.privateStaticMethod(typeof<'TReturn>, name, parameterTypes |> Array.map GenType)

        static member privateStaticMethod<'TReturn>(name) = 
            IL.privateStaticMethod(typeof<'TReturn>, name, [||])

        static member privateStaticMethod(returnType, name) = 
            IL.privateStaticMethod(GenType returnType, name, [||])

        // public static
        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.defineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, returnType, parameterTypes)
            
        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(ClrType returnType, name,  parameterTypes)

        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(GenType returnType, name, parameterTypes)

        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(ClrType returnType, name, parameterTypes |> Array.map ClrType)

        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(GenType returnType, name, parameterTypes |> Array.map ClrType)

        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(ClrType returnType, name, parameterTypes |> Array.map GenType)

        static member publicStaticMethod(returnType, name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(GenType returnType, name, parameterTypes |> Array.map GenType)

        static member publicStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(typeof<'TReturn>, name, parameterTypes)

        static member publicStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(typeof<'TReturn>, name, parameterTypes |> Array.map ClrType)

        static member publicStaticMethod<'TReturn>(name, [<ParamArray>] parameterTypes) = 
            IL.publicStaticMethod(typeof<'TReturn>, name, parameterTypes |> Array.map GenType)

        static member publicStaticMethod<'TReturn>(name) = 
            IL.publicStaticMethod(typeof<'TReturn>, name, [||])

        static member publicStaticMethod(returnType, name) = 
            IL.publicStaticMethod(GenType returnType, name, [||])

        // exported
        static member publicStaticMethod_Exported(methodName, returnType, parameters, exportIndex) = 
            IKVMMethodBuilder(methodName, MethodAttributes.Public ||| MethodAttributes.Static, returnType, parameters, export = (methodName, exportIndex))

        // extension
        static member extensionMethod returnType methodName parameters = 
            let ci = typeof<System.Runtime.CompilerServices.ExtensionAttribute>.GetConstructor([||])
            IKVMMethodBuilder(methodName, MethodAttributes.Public ||| MethodAttributes.Static, returnType, parameters |> Seq.toArray, ci)
            
            
    
        (*
         * Fields
         *)
        static member privateStaticField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(fieldType), FieldAttributes.Static ||| FieldAttributes.Private)         

        static member privateStaticField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, fieldType, FieldAttributes.Static ||| FieldAttributes.Private)         

        static member privateStaticField<'TField> name =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(typeof<'TField>), FieldAttributes.Static ||| FieldAttributes.Private)         

        static member publicStaticField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(fieldType), FieldAttributes.Static ||| FieldAttributes.Public)         

        static member publicStaticField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, fieldType, FieldAttributes.Static ||| FieldAttributes.Public)         

        static member publicStaticField<'TField> name =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(typeof<'TField>), FieldAttributes.Static ||| FieldAttributes.Public)         

        static member publicField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(fieldType), FieldAttributes.Public)         

        static member publicField<'TField> name =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(typeof<'TField>), FieldAttributes.Public)         

        static member publicField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, fieldType, FieldAttributes.Public)         

        static member privateField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, fieldType, FieldAttributes.Private)         

        static member privateField (fieldType, name) =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(fieldType), FieldAttributes.Private)         

        static member privateField<'TField> name =
            fun (u : Universe, tb : TypeBuilder) ->
                tb.DefineField(name, u.Import(typeof<'TField>), FieldAttributes.Private)         
        
        (*
         * Properties
         *)
        static member publicAutoProperty(propType, name) = 
            IKVMAutoPropertyBuilder(name, PropertyAttributes.None, ClrType propType, [||])

        static member publicAutoProperty<'TProperty>(name) = 
            IL.publicAutoProperty(typeof<'TProperty>, name)

        static member publicAutoProperty(propType, name) = 
            IKVMAutoPropertyBuilder(name, PropertyAttributes.None, GenType propType, [||])

        static member publicProperty(propType, name) = 
            IKVMPropertyBuilder(name, PropertyAttributes.None, ClrType propType, [||])

        static member publicProperty<'TProperty> name = 
            IL.publicProperty(typeof<'TProperty>, name)

        static member publicProperty(propType, name) = 
            IKVMPropertyBuilder(name, PropertyAttributes.None, GenType propType, [||])


