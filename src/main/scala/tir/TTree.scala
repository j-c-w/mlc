package tir

import toplev.GenericPrintable

trait TTree extends GenericPrintable {
  def nodeClone: TTree
}
