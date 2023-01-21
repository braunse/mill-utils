/*
 * Copyright (C) 2022 Sebastien Braun.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package de.sebbraun.millutils

import mill._
import mill.define._
import mill.scalajslib._
import mill.scalalib._

import scala.language.implicitConversions

trait SharedModule extends Module { mod =>

  def scalaVersion: String
  def scalaJSVersion: String

  def ivyDeps: T[Agg[MultiDep]]        = T { Agg() }
  def frontendIvyDeps: T[Agg[Dep]]     = T { Agg() }
  def backendIvyDeps: T[Agg[Dep]]      = T { Agg() }
  def testIvyDeps: T[Agg[MultiDep]]    = T { Agg() }
  def frontendTestIvyDeps: T[Agg[Dep]] = T { Agg() }
  def backendTestIvyDeps: T[Agg[Dep]]  = T { Agg() }

  def moduleDeps: Seq[SharedModule]       = Seq()
  def backendModuleDeps: Seq[Module]      = Seq()
  def frontendModuleDeps: Seq[Module]     = Seq()
  def testModuleDeps: Seq[SharedModule]   = Seq()
  def backendTestModuleDeps: Seq[Module]  = Seq()
  def frontendTestModuleDeps: Seq[Module] = Seq()

  def sharedSources: Sources     = T.sources(millSourcePath / "src-shared")
  def frontendSources: Sources   = T.sources(millSourcePath / "src-frontend")
  def backendSources: Sources    = T.sources(millSourcePath / "src-backend")
  def sharedResources: Sources   = T.sources(millSourcePath / "res-shared")
  def frontendResources: Sources = T.sources(millSourcePath / "res-frontend")
  def backendResources: Sources  = T.sources(millSourcePath / "res-backend")

  def sharedTestSources: Sources   = T.sources(millSourcePath / "test-shared")
  def frontendTestSources: Sources = T.sources(millSourcePath / "test-frontend")
  def backendTestSources: Sources  = T.sources(millSourcePath / "test-backend")
  def sharedTestResources: Sources =
    T.sources(millSourcePath / "test-res-shared")
  def frontendTestResources: Sources =
    T.sources(millSourcePath / "test-res-frontend")
  def backendTestResources: Sources =
    T.sources(millSourcePath / "test-res-backend")

  def testFramework: T[String]         = T { "<NONE>" }
  def frontendTestFramework: T[String] = T { testFramework() }
  def backendTestFramework: T[String]  = T { testFramework() }

  trait FrontendModule extends ScalaJSModule { frontend =>
    private def mapModuleDep(m: Module): ScalaJSModule = m match {
      case sh: SharedModule  => sh.frontend
      case js: ScalaJSModule => js
      case _ =>
        throw new IllegalArgumentException(
          s"$m must be a SharedModule or a ScalaJSModule"
        )
    }

    def scalaVersion            = mod.scalaVersion
    def scalaJSVersion          = mod.scalaJSVersion
    override def millSourcePath = mod.millSourcePath

    override def sources = T.sources(
      sharedSources() ++ frontendSources()
    )

    override def resources = T.sources(
      sharedResources() ++ frontendResources()
    )

    override def ivyDeps = T {
      mod.ivyDeps().map(_.js) ++ mod.frontendIvyDeps()
    }

    override def moduleDeps =
      (mod.moduleDeps ++ mod.frontendModuleDeps).map(mapModuleDep)

    trait FrontendTests extends super.Tests {
      override def millSourcePath = mod.millSourcePath

      override def testFramework = T { frontendTestFramework() }

      override def sources = T.sources(
        sharedTestSources() ++ frontendTestSources()
      )

      override def resources = T.sources(
        sharedTestResources() ++ frontendTestResources()
      )

      override def ivyDeps = T {
        testIvyDeps().map(_.js) ++ frontendTestIvyDeps()
      }

      override def moduleDeps =
        Seq(frontend) ++
          (testModuleDeps ++ frontendTestModuleDeps).map(mapModuleDep)
    }
  }

  val frontend: FrontendModule

  trait BackendModule extends ScalaModule { backend =>
    private def mapModuleDep(m: Module): ScalaModule = m match {
      case sm: SharedModule => sm.backend
      case sm: ScalaModule  => sm
      case _ =>
        throw new IllegalArgumentException(
          s"$m must be a SharedModule or a ScalaModule"
        )
    }

    def scalaVersion            = mod.scalaVersion
    override def millSourcePath = mod.millSourcePath

    override def sources = T.sources(
      sharedSources() ++ backendSources()
    )

    override def resources = T.sources(
      sharedResources() ++ backendResources()
    )

    override def ivyDeps = T {
      mod.ivyDeps().map(_.jvm) ++ mod.backendIvyDeps()
    }

    override def moduleDeps =
      mod.moduleDeps.map(mapModuleDep) ++
        mod.backendModuleDeps.map(mapModuleDep)

    trait BackendTests extends super.Tests {
      override def millSourcePath = mod.millSourcePath

      override def testFramework = T { backendTestFramework() }

      override def sources = T.sources(
        sharedTestSources() ++ backendTestSources()
      )

      override def resources = T.sources(
        sharedTestResources() ++ backendTestResources()
      )

      override def ivyDeps = T {
        testIvyDeps().map(_.jvm) ++ backendTestIvyDeps()
      }

      override def moduleDeps =
        Seq(backend) ++
          testModuleDeps.map(mapModuleDep) ++
          backendTestModuleDeps.map(mapModuleDep)
    }
  }

  val backend: BackendModule
}

trait SharedModuleImplicits {
  implicit def sharedModuleAsBackend(m: SharedModule): m.BackendModule =
    m.backend
  implicit def sharedModuleAsFrontend(m: SharedModule): m.FrontendModule =
    m.frontend
}
