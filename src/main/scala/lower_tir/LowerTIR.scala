package lower_tir

import byteR._
import exceptions.ICE
import scala.collection.mutable.Set
import tir._
import toplev.Pass

object LowerTIR extends Pass[TJavaProgram, JVMProgram]("lower_tir") {
  def lowerMain(fun: TJavaFun,
                topLevelVariables: Set[TTopLevelIdent]): JVMClass = {
    /* This is a special case. It does not extend function1
     * like all the other functions will.
     *
     * The direct argument count is treated as 0 because we never
     * use the passed command line arguments, so they do not
     * have to be preserved.
     */
    val (stackDepth, functionCode) =
      LowerBody(fun.exp.exp, 0, fun.exp.valdecs.size, fun.env)
    val limitedCode =
      StackLimitDirective(stackDepth) ::
    // We must add one to the local variables size because 0 is reserved for
    // references.
      LocalsLimitDirective(fun.exp.valdecs.size + 1) ::
      functionCode

    val function = JVMMethod("main", List(JVMArrayType(JVMStringType())),
                             JVMVoidPrimitiveType(), limitedCode, true)

    JVMClass("Main", Some(JVMObjectRef()),
             staticFieldsFor(fun.env, topLevelVariables), List(function))
  }

  private def staticFieldsFor(env: TTypeEnv, vals: Set[TTopLevelIdent]) = {
    vals.map { case ident @ TTopLevelIdent(name, identClass) => {
        // Given that we are making this a field, we expect this to be
        // a value type that can go into a field.
        assert(identClass.isRegisterClass)

        JVMField(LowerName(name), LowerType(env.getOrFail(ident)), true)
      }
    }.toList
  }

  /* Use the lowerFunction helper method rather than trying to correctly guess
   * the correct starting arguments for this function.
   *
   * The idea here is to lower functions into the same representation they are
   * lowered into in Scala.
   *
   * That is, some:
   *
   *    fun f x y z = x + y + z
   *
   * Becomes:
   *
   * Class Fun0f extends Fun {
   *    Fun1f apply(Object argument) {
   *     new Fun1f(argument);
   *    }
   * }
   *
   * class Fun1f extends Fun {
   *   Integer x;
   *   Fun1f(Integer x) {this.x = x;}
   *   Fun2f apply(Object argument) {
   *     new Fun2f(x, (Integer)argument);
   *   }
   * }
   *
   * class Fun2f extends Fun {
   *   Integer x, y, z;
   *   Fun2f(Integer x, Integer y) {this.x = x; this.y = y }
   *   Integer apply(Object argument) {
   *     this.z = (Integer) argument;
   *     return x + y + z;
   *   }
   * }
   */
  def recursiveLowerFunction(funExp: TExpFunLet, arguments: List[TType],
                             number: Int, builtupTypes: List[JVMType],
                             name: TTopLevelIdent, typ: TType,
                             env: TTypeEnv): List[JVMClass] = {
    val (argType, returnType) = typ match {
      case TFunctionType(arg, res) => (arg, res)
      case _ => throw new ICE("Function without function type!")
    }

    // Note that the world assumes that this function is going to be accessible
    // through the name "Fun'number'Name".
    val funName = "Fun" + number + LowerName(name.name)
    val thisClassRef = JVMClassRef.classRefFor(LowerName(funName))
    val loweredArgType = LowerType(argType)

    // If there are no local applciations left (e.g. this is the last currying)
    // then add this to the LowerShared map so that local variables know  what
    // class to access through.
    if (arguments.length == 1) {
      LowerShared.functionAccessMap(name) = funName
    }

    val (functionCode, otherClasses) = arguments match {
      case Nil => throw new ICE("Empty function arguments")
      // This is the last application that needs to be considered:
      case List(arg) => {
        // The direct arguments to this function is 1.
        val (stackDepth, expCode) =
          LowerBody(funExp.exp, 1, funExp.valdecs.size, env)
        // We need at least one varibale for the passed variable.
        // However, slot is reused later, just have to ensure it exists.
        // The other slot we need is for the local function reference.
        val localsCount = Math.max(funExp.valdecs.size + 1, 2)
        val argSet = List(
          // The directives need to go first.  Again, the pushing of the
          // argument requires a stack depth of 1.
          StackLimitDirective(Math.max(stackDepth, 1)),
          // Increased by 1 for the self reference.  The space for the passed
          // argument may be reused.
          LocalsLimitDirective(localsCount + 1),
          JVMSelfLoad(),
          // Load the argument
          JVMLocalALoad(1),
          JVMCheckCast(loweredArgType.getRefFor),
          JVMPutField(thisClassRef, "arg" + number, loweredArgType))

        // The code for this function puts the argument in the shared variable
        // and then executes the code.
        (argSet ::: expCode, List())
      }
      case (arg :: args) => {
        val otherClasses =
          recursiveLowerFunction(funExp, args, number + 1,
                                 builtupTypes :+ loweredArgType, name,
                                 returnType, env)
        val argSet =
          JVMPutField(thisClassRef, "arg" + number, loweredArgType)

        // Initialze the next function:
        val instrs =
          // +1 for the added argument, + 2 for the duplicated class ref.
          (StackLimitDirective(number + 1 + 2) ::
          // One for the self reference, one for the passed argument.
           LocalsLimitDirective(2) ::
           JVMNew(JVMClassRef.classRefFor(
             LowerName(otherClasses.head.name))) ::
           JVMDup() ::
          // The code for this function then initializes the head of the
          // otherClasses list
          ((0 until number).map({ (n: Int) =>
             List(JVMSelfLoad(),
                  JVMGetField(thisClassRef, "arg" + n, builtupTypes(n)))
           }).flatten.toList) :::
          // Load the argument onto the top of this stack.  For better or for
          // worse, this index is incremented internally to 1.
          LowerLoadIdent(new TNumberedIdentVar(funName, 0), env)) :+
          // And cast it
          JVMCheckCast(loweredArgType.getRefFor) :+
          // Then  invoke the constructor for  the next level.
           new JVMInvokeSpecialMethod(
             new JVMMethodRef(
               JVMClassRef.classRefFor(LowerName(otherClasses.head.name)),
               "<init>", builtupTypes :+ loweredArgType,
               JVMVoidPrimitiveType()))
        (instrs, otherClasses)
      }
    }

    // The init code has the task of copying all the arguments (of which
    // there are N) into the local variables.
    //
    // Note that it cheats here and uses local variables references that
    // have been created into the local variable array since that is
    // where the arguments are.
    val initCode =
    // There are only ever two things: the value to be stored and the self
    // ref.
    StackLimitDirective(2) ::
    // But we need to have space for number + 1 arguments, + 1 for the
    // self ref.
    LocalsLimitDirective(number + 1 + 1) ::
    // call init:
    JVMSelfLoad() ::
    JVMInvokeSpecialMethod(
      new JVMMethodRef(JVMFunctionRef(), "<init>",
                       List(), JVMVoidPrimitiveType())) ::
    (0 until number).map({ (n: Int) =>
      (JVMSelfLoad() ::
       LowerLoadIdent(new TNumberedIdentVar(funName, n), env)) :+
      JVMPutField(thisClassRef, "arg" + n, builtupTypes(n))
    }).toList.flatten :::
    // We have to add a new unit on the end so that the generator
    // something to pop off
    LowerLoadIdent(new TUnitIdent(), env)

    // This is the list of local variables for the class.
    // Because we have optimized out the local vairable
    // 'arg"number"' for all but the actual application,
    // this generates a redundant variable for each application.
    val localVariables = (0 until number).map({ (n: Int) =>
      JVMField("arg" + n, builtupTypes(n), false)
    }).toList :+ JVMField("arg" + number, loweredArgType, false)

    val initFunction =
      JVMMethod("<init>", builtupTypes, JVMVoidPrimitiveType(),
                initCode, false)

    val function = JVMMethod("apply", List(JVMObjectType()),
                             JVMObjectType(), functionCode, false)
    JVMClass(funName, Some(JVMFunctionRef()), localVariables,
             List(initFunction, function)) :: otherClasses
  }

  def lowerFunction(fun: TJavaFun): List[JVMClass] =
    recursiveLowerFunction(fun.exp, fun.curriedArgs.map(fun.env.getOrFail(_)),
                           0, List(), fun.name,
                           fun.env.getOrFail(fun.name), fun.env)

  def run(tree: TJavaProgram) = {
    // Create a new Java program:
    val main = lowerMain(tree.main, tree.topLevelVariables)
    val functions = tree.functions.map(lowerFunction(_)).flatten

    new JVMProgram(main :: functions)
  }
}
