package de.sebbraun.millutils

import mill.scalalib._
import upickle.default._

case class MultiDep(jvm: Dep, js: Dep)

trait MultiDepImplicits {
  implicit val multiDepRW: ReadWriter[MultiDep] = macroRW
}
