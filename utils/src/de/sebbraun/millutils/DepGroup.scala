/*
 * Copyright (C) 2022 Sebastien Braun.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package de.sebbraun.millutils

import mill.scalalib._

import scala.language.implicitConversions

sealed abstract class LangVersion(val value: String)
object LangVersion {
  case object Java       extends LangVersion(":")
  case object ScalaMajor extends LangVersion("::")
  case object ScalaMinor extends LangVersion(":::")
}

sealed abstract class Platform(val value: String)
object Platform {
  case object Jvm   extends Platform(":")
  case object Other extends Platform("::")
}

case class DepGroup(groupID: String, version: String, prefix: String = "", separator: String = "-") {
  def assemble(
      suffix: String,
      artifactID: String,
      groupID: String,
      version: String,
      langVersion: LangVersion,
      platform: Platform
  ): Dep = {
    val theGroupID = if (groupID != null) groupID else this.groupID
    val theArtifactID =
      if (prefix != null && suffix != null && !prefix.isEmpty() && !suffix.isEmpty()) s"$prefix$separator$suffix"
      else if (suffix != null && !suffix.isEmpty()) suffix
      else if (artifactID != null && !artifactID.isEmpty()) artifactID
      else prefix.stripSuffix(separator)
    val theVersion = if (version != null) version else this.version
    ivy"$theGroupID${langVersion.value}$theArtifactID${platform.value}$theVersion"
  }

  def java(
      suffix: String = null,
      artifactID: String = null,
      groupID: String = null,
      version: String = null
  ): Dep = assemble(
    suffix,
    artifactID,
    groupID,
    version,
    LangVersion.Java,
    Platform.Jvm
  )

  def jvm(
      suffix: String = null,
      artifactID: String = null,
      groupID: String = null,
      version: String = null
  ): Dep = assemble(
    suffix,
    artifactID,
    groupID,
    version,
    LangVersion.ScalaMajor,
    Platform.Jvm
  )

  def js(
      suffix: String = null,
      artifactID: String = null,
      groupID: String = null,
      version: String = null
  ): Dep = assemble(
    suffix,
    artifactID,
    groupID,
    version,
    LangVersion.ScalaMajor,
    Platform.Other
  )

  def multi(
      suffix: String = null,
      artifactID: String = null,
      groupID: String = null,
      version: String = null
  ): MultiDep = MultiDep(
    jvm = jvm(suffix, artifactID, groupID, version),
    js = js(suffix, artifactID, groupID, version)
  )

  private[millutils] lazy val selfArtifactID = prefix match {
    case "" =>
      throw new IllegalStateException(
        "Prefix must be given to use the `self` object"
      )
    case s => s.stripSuffix("-")
  }

  lazy val self: MultiDep = multi()
}

trait DepGroupImplicits {
  implicit def depGroupAsMultiDep(depGroup: DepGroup): MultiDep = depGroup.self
}
