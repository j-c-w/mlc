package function_analysis

import tir._

object FunctionAnalysis {
  def getFullApplicatonFrom(name: TNamedIdent, curriedLength: Int,
                            env: TTypeEnv,
                            app: TExp): Option[List[(TExp, TType)]] = {
    def applicationSearch(app: TExp, curriedArgs: Int, exps: List[TExp],
                          typs: List[TType]): Option[List[(TExp, TType)]] =
      app match {
        case TExpIdent(applicationName) =>
          if (applicationName == name && curriedArgs == 0) {
            assert(exps.length == typs.length)
            Some(exps zip typs)
          } else {
            None
          }
        case TExpFunApp(fun, appExp, typ) =>
          if (curriedArgs == 0)
            None
          else {
            applicationSearch(fun, curriedArgs - 1, appExp :: exps,
                              env.getNCurriedTypesFrom(1, typ) ::: typs)
          }
        case _ => None
      }

    applicationSearch(app, curriedLength, List(), List())
  }
}
