package lower_tir

import exceptions.ICE
import byteR._
import tir._

object LowerExp {
  def apply(exp: TExp, env: TTypeEnv): List[JVMInstruction] = exp match {
    case TExpConst(const: TConst) => LowerConst(const)
    // This is a 'load' by default because the only way to store into
    // an ident is in an assign node, so that is handled as a special case.
    case TExpIdent(ident: TIdent) => LowerLoadIdent(ident, env)
    // There are a lot of special cases for function applications
    // that can be compiled directly to JVM instructions rather
    // than function calls.
    // List operations:
    case TExpFunApp(TExpIdent(TConsIdent()), application, typ) => {
      // Build the tuple, extract it and get the arguments:
      extractNTuple(application, env, 2,
                    List(JVMObjectType(), JVMLinkedListType()), false) :+
      JVMSwap() :+
      // Now call the cons method on the list class
      listConsCall
    }
    case TExpFunApp(TExpIdent(TAppendIdent()), application, typ) => {
      // Build the tuple and extract it to get the arguments:
      extractNTuple(application, env, 2,
                    List(JVMObjectType(), JVMObjectType()), false) :+
      // Now call the append method on the list class
      JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMLinkedListRef(), "append",
                         List(JVMLinkedListType()), JVMLinkedListType()))
    }
    // Real operations:
    case TExpFunApp(TExpIdent(TRealPlusIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env, List(JVMFAdd()))
    case TExpFunApp(TExpIdent(TRealMinusIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env, List(JVMFSub()))
    case TExpFunApp(TExpIdent(TRealTimesIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env, List(JVMFMul()))
    case TExpFunApp(TExpIdent(TRealDivIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env, List(JVMFDiv()))
    // Int operations:
    case TExpFunApp(TExpIdent(TIntPlusIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env, List(JVMIAdd()))
    case TExpFunApp(TExpIdent(TIntMinusIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env, List(JVMISub()))
    case TExpFunApp(TExpIdent(TIntTimesIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env, List(JVMIMul()))
    case TExpFunApp(TExpIdent(TIntDivIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env, List(JVMIDiv()))
    case TExpFunApp(TExpIdent(TIntModIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env, List(JVMIRem()))
    // String operations:
    case TExpFunApp(TExpIdent(TStringCatIdent()), application, typ) => {
      // As in Java, this is done using a string builder.
      (extractNTuple(application, env, 2,
                     List(new JVMStringType(), new JVMStringType()), false)) :+
      // String builders are basically linked lists.  Anything that is not
      // 'append' makes string builders go very slow.  So, we swap the strings
      // on the stack here to enable correct use of 'append'.
      JVMSwap() :+
      // Make the string builder:
      JVMNew(JVMStringBuilderRef()) :+
      JVMDup() :+
      JVMInvokeSpecialMethod(
        new JVMMethodRef(JVMStringBuilderRef(), "<init>", List(),
                         JVMVoidPrimitiveType())) :+
      // Now, swap and apply twice:
      JVMSwap() :+
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMStringBuilderRef(), "append",
                         List(JVMStringType()), JVMStringBuilderType())) :+
      JVMSwap() :+
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMStringBuilderRef(), "append",
                         List(JVMStringType()), JVMStringBuilderType())) :+
      // And finally extract the string from it:
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMStringBuilderRef(), "toString", List(),
                         JVMStringType()))
    }
    // Real comparison operations:
    case TExpFunApp(TExpIdent(TRealLEQIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env,
        JVMFCmpG() ::
        JVMIPush(1) ::
        // If this is equal to 1, then it was strictly greater than, so
        // return false.
        ifIntEq(List(JVMIPush(0)), List(JVMIPush(1))),
                           JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TRealGEQIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env,
        JVMFCmpL() ::
        JVMIPush(-1) ::
        // If this is equal to -1, then it was strictly less than, so
        // return false.
        ifIntEq(List(JVMIPush(0)), List(JVMIPush(1))),
                           JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TRealLTIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env,
        JVMFCmpG() ::
        JVMIPush(-1) ::
        // If this is equal to -1, then it was strictly less than, so
        // return true.
        ifIntEq(List(JVMIPush(1)), List(JVMIPush(0))),
                           JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TRealGTIdent()), application, typ) =>
      boxedFloatOperation(application, 2, env,
        JVMFCmpL() ::
        JVMIPush(1) ::
        // If this is equal to 1, then it was strictly greater than, so
        // return true.
        ifIntEq(List(JVMIPush(1)), List(JVMIPush(0))),
                           JVMBooleanPrimitiveType())
    // Char comparison operations:
    case TExpFunApp(TExpIdent(TCharLEQIdent()), application, typ) =>
      boxedCharCmp(application, env, JVMIfIntCmpLEQ)
    case TExpFunApp(TExpIdent(TCharGEQIdent()), application, typ) =>
      boxedCharCmp(application, env, JVMIfIntCmpGEQ)
    case TExpFunApp(TExpIdent(TCharLTIdent()), application, typ) =>
      boxedCharCmp(application, env, JVMIfIntCmpLT)
    case TExpFunApp(TExpIdent(TCharGTIdent()), application, typ) =>
      boxedCharCmp(application, env, JVMIfIntCmpGT)
    // Int comparison operations:
    case TExpFunApp(TExpIdent(TIntLEQIdent()), application, typ) =>
      boxedIntCmp(application, env, JVMIfIntCmpLEQ)
    case TExpFunApp(TExpIdent(TIntGEQIdent()), application, typ) =>
      boxedIntCmp(application, env, JVMIfIntCmpGEQ)
    case TExpFunApp(TExpIdent(TIntLTIdent()), application, typ) =>
      boxedIntCmp(application, env, JVMIfIntCmpLT)
    case TExpFunApp(TExpIdent(TIntGTIdent()), application, typ) =>
      boxedIntCmp(application, env, JVMIfIntCmpGT)
    // String comparison operations:
    case TExpFunApp(TExpIdent(TStringLEQIdent()), application, typ) =>
      (extractNTuple(application, env, 2,
                     List(new JVMStringType(), new JVMStringType()), false) :::
       jvmCompareTo ::
       JVMIPush(0) ::
       unboxedIntCmp(JVMIfIntCmpLEQ)) :+
      box(JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TStringGEQIdent()), application, typ) =>
      (extractNTuple(application, env, 2,
                     List(new JVMStringType(), new JVMStringType()), false) :::
       jvmCompareTo ::
       JVMIPush(0) ::
       unboxedIntCmp(JVMIfIntCmpGEQ)) :+
      box(JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TStringLTIdent()), application, typ) =>
      (extractNTuple(application, env, 2,
                     List(new JVMStringType(), new JVMStringType()), false) :::
       jvmCompareTo ::
       JVMIPush(0) ::
       unboxedIntCmp(JVMIfIntCmpLT)) :+
      box(JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TStringGTIdent()), application, typ) =>
      (extractNTuple(application, env, 2,
                     List(new JVMStringType(), new JVMStringType()), false) :::
       jvmCompareTo ::
       JVMIPush(0) ::
       unboxedIntCmp(JVMIfIntCmpGT)) :+
      box(JVMBooleanPrimitiveType())
    // Equality operations:
    case TExpFunApp(TExpIdent(TGenericEqualsIdent()), application, typ) => {
      // De-tuple the arguments:
      (extractNTuple(application, env, 2,
                     List(new JVMObjectType(), new JVMObjectType()), false)) :+
      JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMObjectRef(), "equals",
                         List(JVMObjectType()), JVMBooleanPrimitiveType())) :+
      box(JVMBooleanPrimitiveType())
    }
    case TExpFunApp(TExpIdent(TRealEqualsIdent()), application, typ) => {
      val ifTrue = LabelGenerator.newLabel()
      val endIf = LabelGenerator.newLabel()

      // De-tuple the arguments:
      (extractNTuple(application, env, 2,
                     List(new JVMFloatType(), new JVMFloatType()), true)) :+
      // Do the actual comparison:
      // The comparison operators return 0 if the arguments are equal.
      JVMFCmpG() :+
      JVMIPush(0) :+
      JVMIfIntCmpEq(ifTrue) :+
      JVMIPush(1) :+
      JVMJump(endIf) :+
      JVMLabelMark(ifTrue) :+
      JVMIPush(0) :+
      JVMLabelMark(endIf) :+
      box(JVMBooleanPrimitiveType())
    }
    case TExpFunApp(TExpIdent(TBoolEqualsIdent()), application, typ) =>
      boxedBooleanOperation(application, 2, env,
                            ifIntEq(List(JVMIPush(1)), List(JVMIPush(0))))
    case TExpFunApp(TExpIdent(TIntEqualsIdent()), application, typ) =>
      boxedIntegerOperation(application, 2, env,
                            ifIntEq(List(JVMIPush(1)), List(JVMIPush(0))),
                            JVMBooleanPrimitiveType())
    case TExpFunApp(TExpIdent(TCharEqualsIdent()), application, typ) => {
      boxedCharOperation(application, 2, env,
                         ifIntEq(List(JVMIPush(1)), List(JVMIPush(0))),
                         JVMBooleanPrimitiveType())
    }
    case TExpFunApp(TExpIdent(TStringEqualsIdent()), application, typ) => {
      stringOperation(application, 2, env, List(equals(JVMStringType())),
                      JVMBooleanType()) :+
      // Need to box this.
      box(JVMBooleanPrimitiveType())
    }
    // Boolean operations: These are the only two lazy operations.
    case TExpFunApp(TExpIdent(TAnd()), TExpTuple(List(item1, item2)), typ) =>
      // Convert this to If(item1) then item2 else false
      LowerExp(TExpIf(item1, item2, TExpConst(TConstFalse())), env)
    case TExpFunApp(TExpIdent(TOr()), TExpTuple(List(item1, item2)), typ) =>
      // This becomes If(item1) then true else item2
      LowerExp(TExpIf(item1, TExpConst(TConstTrue()), item2), env)
    // Negation operations:
    case TExpFunApp(TExpIdent(TNegInt()), application, typ) =>
      boxedIntegerOperation(application, 1, env, List(JVMINeg()))
    case TExpFunApp(TExpIdent(TNegReal()), application, typ) =>
      boxedFloatOperation(application, 1, env, List(JVMFNeg()))
    case TExpFunApp(TExpIdent(TNot()), application, typ) => {
      boxedBooleanOperation(application, 1, env,
        // Now branch and load a 0 or a 1.
        JVMIPush(1) ::
        ifIntEq(List(JVMIPush(0)), List(JVMIPush(1))))
    }
    // Printing:
    case TExpFunApp(TExpIdent(TPrint()), application, typ) => {
      // Load the PrintStream:
      ((JVMGetStaticField(new JVMSystemRef(), "out",
                         new JVMPrintStreamType()) ::
        LowerExp(application, env)) :+
      // And invoke:
       JVMInvokeVirtualMethod(new JVMMethodRef(new JVMPrintStreamRef(),
                                               "print",
                                               List(new JVMStringType()),
                                               JVMVoidPrimitiveType()))) :::
      // Finally, since we said that this pushed unit, we create a new Unit:
      LowerLoadIdent(new TUnitIdent(), env)
    }
    case TExpFunApp(function, application, typ) => {
      val funInstrs = apply(function, env)
      val applicationInstrs = apply(application, env)

      val resultType = env.getOrFail(typ) match {
        case TFunctionType(from, to) => LowerType(to)
        case _ => throw new ICE("""A function application must have a function
          |type. Instead found %s""".stripMargin.format(typ.prettyPrint))
      }

      val methodRef =
        JVMMethodRef(JVMFunctionRef(), "apply",
                     List(JVMObjectType()), JVMObjectType())

      funInstrs :::
      applicationInstrs :::
      List(JVMInvokeVirtualMethod(methodRef),
          // Then we must cast the function result to the
          // desired type.
           JVMCheckCast(resultType.getRefFor))
    }
    case TExpTuple(subExps) => {
      val tupleLength = subExps.length

      JVMNew(JVMTupleRef()) ::
      // Need one instance to invoke the constructor.
      JVMDup() ::
      JVMIPush(tupleLength) ::
      JVMInvokeSpecialMethod(
        new JVMMethodRef(JVMTupleRef(), "<init>",
                         List(JVMIntPrimitiveType()),
                         JVMVoidPrimitiveType())) ::
      (((1 to tupleLength) zip subExps).map {
      // Duplicate the tuple ref, lower the expression, then insert the
      // variable.
        case (n, exp) => (JVMDup() :: JVMIPush(n - 1) :: LowerExp(exp, env)) :+
            // Then add that into the tuple
            JVMInvokeVirtualMethod(
              new JVMMethodRef(JVMTupleRef(), "set",
                               List(JVMIntPrimitiveType(), JVMObjectType()),
                               JVMVoidPrimitiveType()))
      }).toList.flatten
    }
    case TExpList(elements) => {
      // Build the stack from each of the elements compiled on its own.
      // Start with the last element and build inwards.  We can keep the stack
      // length down by interleaving the list creating with the element
      // generation.

      // Push nil
      LowerLoadIdent(new TEmptyListIdent(), env) :::
      // We have to reverse the list so that the last item is added first.
      elements.reverse.map {
        case (exp) =>
          LowerExp(exp, env) :+
          // Below that element should be a list
          listConsCall
      }.flatten
    }
    case TExpSeq(sequence) =>
      // We have to insert 'Pops' between each of the statements,
      // but not a pop at the end.
      sequence.map(LowerExp.apply(_, env)).
        map(x => x :+ new JVMPop()).flatten.dropRight(1)
    case TExpAssign(identVar, exp) =>
      LowerExp(exp, env) ::: LowerStoreIdent(identVar, env) :::
      LowerLoadIdent(new TUnitIdent(), env)
    case TExpListHead(listExp, typ) =>
      LowerExp(listExp, env) :+
      JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMLinkedListRef(), "head",
                         List(), JVMObjectType())) :+
      // And cast it
      JVMCheckCast(LowerType(env.getOrFail(typ)).getRefFor)
    case TExpListTail(listExp) =>
      LowerExp(listExp, env) :+
      new JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMLinkedListRef(), "tail",
                         List(), JVMLinkedListType()))
    case TExpTupleExtract(tupleExp, tupleSize, index, typ) => {
      // Idea is the get the code for the tuple and then extract the
      // right field from it.
      //
      // We know that the top of stack is a tuple of the right size,
      // so we can just extract from that.
      LowerExp(tupleExp, env) :+
      JVMIPush(index) :+
      new JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMTupleRef(), "get",
                         List(new JVMIntPrimitiveType()),
                         new JVMObjectType())) :+
      // Then, we have to case the result object to the right type.
      new JVMCheckCast(LowerType(env.getOrFail(typ)).getRefFor)
    }
    case TExpListExtract(listExp, index, typ) => {
      // Compile the list:
      LowerExp(listExp, env) :+
      // Push the index
      JVMIPush(index) :+
      // And invoke the method:
      JVMInvokeVirtualMethod(
        new JVMMethodRef(new JVMLinkedListRef(), "at",
                         List(JVMIntPrimitiveType()), JVMObjectType())) :+
      // Cast the object:
      JVMCheckCast(LowerType(env.getOrFail(typ)).getRefFor)
    }
    case TExpListLength(listExp) => {
      // Compile the list
      LowerExp(listExp, env) :+
      // Invoke the mehtod
      JVMGetField(new JVMLinkedListRef(), "length", JVMIntPrimitiveType()) :+
      box(JVMIntPrimitiveType())
    }
    case TExpIf(cond, ifTrue, ifFalse) => {
      val falseLabel = LabelGenerator.newLabel()
      var endLabel = LabelGenerator.newLabel()

      LowerExp(cond, env) :::
      List(unbox(JVMBooleanType())) :::
      List(new JVMIPush(0), new JVMIfIntCmpEq(falseLabel)) :::
      LowerExp(ifTrue, env) :::
      List(JVMJump(endLabel),
           JVMLabelMark(falseLabel)) :::
      LowerExp(ifFalse, env) :::
      List(JVMLabelMark(endLabel))
    }
    case TExpWhile(cond, body, loopID) => {
      val label = LabelGenerator.labelFor(loopID)
      val loopEndLabel = LabelGenerator.newLabel()

      (JVMLabelMark(label) ::
       LowerExp(cond, env)) :::
      List(unbox(JVMBooleanType()),
           new JVMIPush(0),
           new JVMIfIntCmpEq(loopEndLabel)) :::
      (LowerExp(body, env) :+ JVMPop() :+
       JVMLabelMark(loopEndLabel)) :::
      // Push a unit to keep stack sizes consistent.
      LowerExp(TExpIdent(TUnitIdent()), env)
    }
    case TExpReturn(value) => {
      // Push the unit on the end to keep the stack sizes
      // correct.
      (LowerExp(value, env) :+ JVMAReturn()) :::
       LowerExp(TExpIdent(TUnitIdent()), env)
      // ::: LowerExp(TExpIdent(TUnitIdent()), env)
    }
    case TExpContinue(loopID) => {
      val label = LabelGenerator.labelFor(loopID)

      List(JVMJump(label)) ::: LowerExp(TExpIdent(TUnitIdent()), env)
    }
    case TExpThrow(throwable) =>
      LowerLoadIdent(throwable, env) :+ new JVMAThrow()
    case TExpLetIn(_, _, _) => throw new ICE("""Error: Expected the TExpLetIn
      |to have been remvoved. It was not""".stripMargin)
    case TExpFn(_, _) => throw new ICE("""Error: Expected TExpFn to have
      |been removed. It was not""".stripMargin)
    case TExpFunLet(_, _) => throw new ICE("""Error: LowerExp cannot
      |lower FunLets""".stripMargin)
    case TExpCase(_, _, _) => throw new ICE("""Error: Expected case statments
      |to have been replaced""".stripMargin)
    case TExpMatchRow(_, _, _) => throw new ICE("""Error: Excpected match
      |rows to have been removed""".stripMargin)
  }

  def boxedIntCmp(application: TExp, env: TTypeEnv,
                  comparison: JVMLabel => JVMInstruction) = {
      boxedIntegerOperation(application, 2, env, unboxedIntCmp(comparison),
                            JVMBooleanPrimitiveType())
  }

  def unboxedIntCmp(comparison: JVMLabel => JVMInstruction) = {
      val ifTrue = LabelGenerator.newLabel()
      val endIf = LabelGenerator.newLabel()

      List(
        comparison(ifTrue),
        JVMIPush(0),
        JVMJump(endIf),
        JVMLabelMark(ifTrue),
        JVMIPush(1),
        JVMLabelMark(endIf)
      )
  }

  def boxedCharCmp(application: TExp, env: TTypeEnv,
                   comparison: JVMLabel => JVMInstruction) = {
    boxedCharOperation(application, 2, env, unboxedIntCmp(comparison),
                       JVMBooleanPrimitiveType())
  }

  def ifIntEq(ifTrue: List[JVMInstruction],
              ifFalse: List[JVMInstruction]) = {
    val trueLabel = LabelGenerator.newLabel()
    val endIf = LabelGenerator.newLabel()
    JVMIfIntCmpEq(trueLabel) ::
    // If False
    ifFalse :::
    JVMJump(endIf) ::
    JVMLabelMark(trueLabel) ::
    ifTrue :::
    List(JVMLabelMark(endIf))
  }

  /* This is for 'easy' debugging.  It allows the easy insertion of a print
   * call for objects.
   *
   * It MUST NOT be used in actual compilation, because it does not push
   * a unit! */
  def debugPrintObject = List(
      JVMGetStaticField(new JVMSystemRef(), "out",
                         new JVMPrintStreamType()),
      JVMSwap(),
      JVMInvokeVirtualMethod(new JVMMethodRef(new JVMPrintStreamRef(),
                                               "println",
                                               List(new JVMObjectType()),
                                               JVMVoidPrimitiveType()))
  )

  /* This extracts a commonly used pattern in the lowering:
   *    - Get the code for the arguments
   *    - Untuple the argument types (skip if they are singleton arguments)
   *      and unbox them
   *    - Apply some instructions
   *    - Rebox
   */
  def boxedOperation(application: TExp, arity: Int, env: TTypeEnv,
                     argumentType: JVMType, resultType: JVMPrimitiveType,
                     instructions: List[JVMInstruction]) = {
    if (arity > 1) {
      // De-tuple
      (extractNTuple(application, env, arity,
                     List.fill(arity)(argumentType), true)) :::
      // Apply the operation:
      (instructions :+
      // And rebox
       box(resultType))
    } else {
      assert(arity == 1)
      ((LowerExp(application, env)) :+
      // Unbox
       unbox(argumentType)) :::
      // Instructions
      (instructions :+
       box(resultType))
    }
  }

  /* This does the same as the above function, but for object types that
   * should not be boxed/unboxed.
   */
  def objectOperation(application: TExp, arity: Int, env: TTypeEnv,
                      argType: JVMType, resType: JVMType,
                      instructions: List[JVMInstruction]) = {
    if (arity > 1) {
      // De-tuple
      extractNTuple(application, env, arity, List.fill(arity)(argType),
                    false) :::
      instructions
    } else {
      assert(arity == 1)
      LowerExp(application, env) :::
      instructions
    }
  }

  def boxedFloatOperation(application: TExp, arity: Int, env: TTypeEnv,
                           instructions: List[JVMInstruction])
      : List[JVMInstruction] =
    boxedFloatOperation(application, arity, env, instructions,
                         new JVMFloatPrimitiveType())

  def boxedFloatOperation(application: TExp, arity: Int, env: TTypeEnv,
                           instructions: List[JVMInstruction],
                           resType: JVMPrimitiveType)
      : List[JVMInstruction] =
    boxedOperation(application, arity, env, new JVMFloatType(),
                   resType, instructions)

  def boxedCharOperation(application: TExp, arity: Int, env: TTypeEnv,
                         instructions: List[JVMInstruction])
      : List[JVMInstruction] =
    boxedCharOperation(application, arity, env, instructions,
                       new JVMCharPrimitiveType())

  def boxedCharOperation(application: TExp, arity: Int, env: TTypeEnv,
                         instructions: List[JVMInstruction],
                         resType: JVMPrimitiveType)
      : List[JVMInstruction] =
    boxedOperation(application, arity, env, new JVMCharacterType(),
                   resType, instructions)

  def boxedBooleanOperation(application: TExp, arity: Int, env: TTypeEnv,
                            instructions: List[JVMInstruction])
      : List[JVMInstruction] =
    boxedBooleanOperation(application, arity, env, instructions,
                          new JVMBooleanPrimitiveType())

  def boxedBooleanOperation(application: TExp, arity: Int, env: TTypeEnv,
                            instructions: List[JVMInstruction],
                            resType: JVMPrimitiveType)
      : List[JVMInstruction] =
    boxedOperation(application, arity, env, new JVMBooleanType(),
                   resType, instructions)

  def boxedIntegerOperation(application: TExp, arity: Int, env: TTypeEnv,
                            instructions: List[JVMInstruction])
      : List[JVMInstruction] =
    boxedIntegerOperation(application, arity, env, instructions,
                          new JVMIntPrimitiveType())

  def boxedIntegerOperation(application: TExp, arity: Int, env: TTypeEnv,
                            instructions: List[JVMInstruction],
                            resType: JVMPrimitiveType)
      : List[JVMInstruction] =
    boxedOperation(application, arity, env, new JVMIntegerType(),
                   resType, instructions)

  def stringOperation(application: TExp, arity: Int, env: TTypeEnv,
                      instructions: List[JVMInstruction])
      : List[JVMInstruction] =
    stringOperation(application, arity, env, instructions, new JVMStringType())

  def stringOperation(application: TExp, arity: Int, env: TTypeEnv,
                      instructions: List[JVMInstruction],
                      resType: JVMType): List[JVMInstruction] =
    objectOperation(application, arity, env, new JVMStringType(),
                    resType, instructions)

  /* This function returns a list of JVM Instructions that expand the tuple
   * elements onto the stack.
   *
   * For example, given: (I, I, S) =>
   *
   * dup
   * get o1
   * casts(1)
   * swap
   * dup
   * get o2
   * casts(2)
   * swap
   * dup
   * get o3
   * swap
   * pop
   */
  private def extractNTuple(application: TExp, env: TTypeEnv,
                            size: Int, types: List[JVMType],
                            extractPrimitives: Boolean) = {
    assert(types.length == size)
    assert(size > 1)
    // This method is not designed to handle parallel tuples.
    // assert(!application.isInstanceOf[TExpParallelTuple])

    application match {
      // Optimize this by not unnessecarily tupling the arguments.
      case TExpTuple(elems) => {
        // It is not possible for this to be a tuple of any other size:
        assert(elems.length == size)

        (((0 until size) zip elems).flatMap {
          case (i, elem) => LowerExp(elem, env) :::
            // If we need to unbox things, that should happen here.
            (if (extractPrimitives) {
               List(unbox(types(i)))
             } else {
               List[JVMInstruction]()
             })
        }).toList
      }
      case _ =>
        // This is not something we can optimize the tupling out of.
        (LowerExp(application, env) :::
         (0 until size).map((i) => {
           List(new JVMDup(),
                new JVMIPush(i),
                JVMInvokeVirtualMethod(
                  new JVMMethodRef(JVMTupleRef(), "get",
                                   List(new JVMIntPrimitiveType()),
                                   new JVMObjectType())),
                JVMCheckCast(types(i).getRefFor)) :::
                (if (extractPrimitives)
                   List(unbox(types(i)))
                 else
                   List()) :::
                (if (types(i).getSizeBytes == 4)
                   List(new JVMSwap())
                 else
                   List(new JVMDup2X1(), new JVMPop2()))
         }).flatten.toList) :+ (new JVMPop())
    }
  }

  def jvmCompareTo =
    JVMInvokeVirtualMethod(
      new JVMMethodRef(new JVMStringRef(), "compareTo",
                       List(JVMStringType()), JVMIntPrimitiveType()))

  def listConsCall =
    JVMInvokeVirtualMethod(
      new JVMMethodRef(new JVMLinkedListRef(), "cons",
                       List(JVMObjectType()), JVMLinkedListType()))

  def unbox(typ: JVMType) = typ match {
    case JVMIntegerType() =>
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMIntegerRef(), "intValue", List(),
                         JVMIntPrimitiveType()))
    case JVMBooleanType() =>
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMBooleanRef(), "booleanValue", List(),
                         JVMBooleanPrimitiveType()))
    case JVMCharacterType() =>
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMCharacterRef(), "charValue", List(),
                         JVMCharPrimitiveType()))
    case JVMFloatType() =>
      JVMInvokeVirtualMethod(
        new JVMMethodRef(JVMFloatRef(), "floatValue", List(),
                         JVMFloatPrimitiveType()))
    case other => throw new ICE("""Cannot extract primitive for type
      |%s""".stripMargin.format(typ.prettyPrint))
  }

  def box(typ: JVMPrimitiveType) = typ match {
    case JVMIntPrimitiveType() =>
      JVMInvokeStaticMethod(new JVMMethodRef(new JVMIntegerRef(), "valueOf",
                            List(JVMIntPrimitiveType()),
                            new JVMIntegerType()))
    case JVMBooleanPrimitiveType() =>
      JVMInvokeStaticMethod(new JVMMethodRef(new JVMBooleanRef(), "valueOf",
                            List(JVMBooleanPrimitiveType()),
                            new JVMBooleanType()))
    case JVMCharPrimitiveType() =>
      JVMInvokeStaticMethod(new JVMMethodRef(new JVMCharacterRef(), "valueOf",
                            List(JVMCharPrimitiveType()),
                            new JVMCharacterType()))
    case JVMFloatPrimitiveType() =>
      JVMInvokeStaticMethod(new JVMMethodRef(new JVMFloatRef(), "valueOf",
                            List(JVMFloatPrimitiveType()),
                            new JVMFloatType()))
    case JVMVoidPrimitiveType() =>
      throw new ICE("Cannot box void")
  }

  def equals(typ: JVMType) =
    JVMInvokeVirtualMethod(new JVMMethodRef(typ.getRefFor, "equals",
                           List(JVMObjectType()), new JVMBooleanPrimitiveType()))
}
