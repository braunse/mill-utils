/*
 * Copyright (C) 2022 Sebastien Braun.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package de.sebbraun.millutils

import mill.scalalib._
import upickle.default._

case class MultiDep(jvm: Dep, js: Dep)

trait MultiDepImplicits {
  implicit val multiDepRW: ReadWriter[MultiDep] = macroRW
}
