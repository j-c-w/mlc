package stdlib

/* If this is taking too much time, then it should be replaced
 * by a compile time map.
 */

import byteR._
import exceptions.ICE
import frontend.ASTType

object StandardLibraries {
  val packages: List[LibraryPackage] = List(
    STDMath,
    STDReal,
    STDTime, STDTimer)

  def apply(names: List[String]): Option[ASTType] = {
    val validPackages = packages map (prefixMatch(names, _))
    
    val validFunctions = validPackages map (_.map{
      case(name, pack) => pack.apply(name) })

    val resultTypes = validFunctions filter (x => x != None && x != Some(None))
    if (resultTypes.length == 0) {
      None
    } else if (resultTypes.length == 1) {
      resultTypes(0).get
    } else {
      throw new ICE("""The standard library identifier %s is ambiguous"""
        .format(names.mkString(".")))
    }
  }

  def loadExpressionFor(names: List[String]) = {
    val classFor = JVMClassRef.classRefFor("cmlc/lib/" + names.mkString("/"))
    List(
      new JVMNew(classFor),
      new JVMDup(),
      new JVMInvokeSpecialMethod(
        new JVMMethodRef(classFor, "<init>", List(), JVMVoidPrimitiveType())))
  }

  private def prefixMatch(names: List[String], pack: LibraryPackage):
      Option[(List[String], LibraryPackage)] = {
    for (acceptedPrefix <- pack.prefixesAccepted) {
      if (names.length > acceptedPrefix.length) {
        if (names.take(acceptedPrefix.length) == acceptedPrefix)
          return Some(names.drop(acceptedPrefix.length), pack)
      }
    }

    None
  }
}
