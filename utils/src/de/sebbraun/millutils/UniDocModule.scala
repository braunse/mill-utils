package de.sebbraun.millutils

import coursier.core.Dependency
import mill._
import mill.api._
import mill.define._
import mill.modules.Jvm
import mill.scalalib._
import mill.scalalib.api.ZincWorkerUtil
import os.Path
import os.proc
import os.write
import coursier.core.Repository
import coursier.Repositories
import coursier.Resolve
import org.apache.tools.ant.taskdefs.optional.depend.Depend

trait UniDocModule extends mill.Module with CoursierModule {

  def scalaVersion: T[String]

  def scalaOrganization: T[String] = T {
    if (ZincWorkerUtil.isDotty(scalaVersion())) "ch.epfl.lamp"
    else "org.scala-lang"
  }

  def scalaDocIvyDeps: T[Agg[Dep]] = T {
    Lib.scalaDocIvyDeps(scalaOrganization(), scalaVersion())
  }

  def scalaDocMainClass: T[String] = T {
    if (ZincWorkerUtil.isScala3(scalaVersion()))
      Result.Success("dotty.tools.scaladoc.Main")
    else Result.Failure("Only Scala 3 supported")
  }

  def scalaDocExternalMappings: T[Seq[String]] = T {
    Seq(
      ".*scala.*::scaladoc3::https://scala-lang.org/api/3.x/",
      ".*java.*::javadoc::https://docs.oracle.com/en/java/javase/19/"
    )
  }

  def scalaDocOptions: T[Seq[String]] = T { Seq.empty[String] }

  def generateInkuire: T[Boolean] = T { true }

  def platformSuffix: T[String] = T { "" }

  override def resolveCoursierDependency: Task[Dep => Dependency] = T.task {
    (d: Dep) =>
      Lib.depToDependency(d, scalaVersion(), platformSuffix())
  }

  def resolveUniDocClasspathDep: Task[Dep => Dependency] = T.task {
    resolveCoursierDependency()
  }

  def resolveUniDocInputDep: Task[Dep => Dependency] = T.task {
    resolveUniDocClasspathDep()
  }

  def customizeUniDocInputDep: Task[Dep => Dep] = T.task { (dep: Dep) =>
    val dep1 = uniDocInputExclusions().iterator.foldLeft(dep) { (dep, gn) =>
      val (g, n, cs) = gn.split(":") match {
        case Array(NES(group), NES(name)) =>
          (group, name, List(CrossVersion.empty(false)))
        case Array(NES(group), "", NES(name)) =>
          (
            group,
            name,
            List(CrossVersion.Binary(false), CrossVersion.Binary(true))
          )
        case Array(NES(group), "", "", NES(name)) =>
          (group, name, List(CrossVersion.Full(false), CrossVersion.Full(true)))
        case _ =>
          throw new IllegalArgumentException(
            s"Exclusion should match group:[:[:]]name format"
          )
      }
      assert(g != "" && n != "")

      dep.exclude(
        cs.map(c =>
          g -> (n + c.suffixString(
            ZincWorkerUtil.scalaBinaryVersion(scalaVersion()),
            scalaVersion(),
            platformSuffix()
          ))
        ): _*
      )
    }

    val dep2 = dep1.excludeOrg(uniDocInputOrgExclusions().iterator.toSeq: _*)

    val dep3 = dep2.excludeName(uniDocInputNameExclusions().iterator.toSeq: _*)

    dep3
  }

  def customizeUniDocClasspathDep: Task[Dep => Dep] = T.task {
    identity[Dep] _
  }

  def uniDocInputExclusions: T[Agg[String]] = T { Agg.empty[String] }

  def uniDocInputOrgExclusions: T[Agg[String]] = T { Agg.empty[String] }

  def uniDocInputNameExclusions: T[Agg[String]] = T { Agg.empty[String] }

  def uniDocIvyDeps: T[Agg[Dep]] = T { Agg.empty[Dep] }

  def uniDocModuleDeps: Seq[ScalaModule] = Seq()

  def ivyDepConflictTask: Task[Iterable[Dep] => Option[Dep]] =
    T.task { (candidates: Iterable[Dep]) =>
      candidates.headOption
    }

  def uniDocInputExclusionFilter: Task[Dep => Boolean] = T.task {
    val gns = uniDocInputExclusions().map(s => ":+".r.split(s) match {
      case Array(group, name) => (group, name)
      case _ => throw new IllegalArgumentException(s"Exclusion $s does not match group:[:[:]]name format")
    }).iterator.toSet
    val gs = uniDocInputOrgExclusions()
    val ns = uniDocInputNameExclusions()

    {
      (d: Dep) =>
        !(
          gns.contains(d.dep.module.organization.value, d.dep.module.name.value) ||
          gs.contains(d.dep.module.organization.value) ||
          ns.contains(d.dep.module.name.value)
        )
    }
  }

  def collectedIvyDeps: T[Agg[Dep]] = T {
    val filterFn = uniDocInputExclusionFilter()

    val ivyDepsFromModules = T
      .traverse(uniDocModuleDeps)(_.transitiveIvyDeps)()
      .flatten
      .filter(filterFn)
    val compileIvyDepsFromModules = T
      .traverse(uniDocModuleDeps)(_.transitiveCompileIvyDeps)()
      .flatten
      .filter(filterFn)

    val allCandidates =
      ivyDepsFromModules ++ compileIvyDepsFromModules ++ uniDocIvyDeps()

    val conflictFn = ivyDepConflictTask()
    allCandidates
      .groupBy(d => d.dep.module.orgName)
      .values
      .flatMap {
        case Seq(unique) => Some(unique)
        case nonUnique   => conflictFn(nonUnique)
      }
  }

  def uniDocModuleCompiledClasses: T[Agg[PathRef]] = T {
    T.traverse(uniDocModuleDeps)(_.compile)().map(_.classes)
  }

  def uniDocInputIvyDeps: T[Agg[Dep]] = T {
    val customizeFn = customizeUniDocInputDep()
    collectedIvyDeps().map(customizeFn)
  }

  def uniDocClasspathIvyDeps: T[Agg[Dep]] = T {
    val customizeFn = customizeUniDocClasspathDep()
    collectedIvyDeps().map(customizeFn)
  }

  def uniDocInputs: T[Agg[PathRef]] = T {
    for {
      ivys <- Lib.resolveDependencies(
        repositories = repositoriesTask(),
        depToDependency = resolveUniDocInputDep(),
        deps = uniDocInputIvyDeps()
      )
    } yield {
      val tasties = Lib
        .findSourceFiles(
          uniDocModuleCompiledClasses().iterator.to(Seq),
          Seq("tasty")
        )
        .map(mill.PathRef(_))

      ivys ++ tasties
    }
  }

  def uniDocClassPath: T[Agg[PathRef]] = T {
    val customizeFn = customizeUniDocClasspathDep()
    for {
      ivys <- Lib.resolveDependencies(
        repositories = repositoriesTask(),
        depToDependency = resolveUniDocClasspathDep(),
        deps = uniDocClasspathIvyDeps()
      )
    } yield {
      ivys ++
        uniDocModuleCompiledClasses()
    }
  }

  def scalaDocClasspath: T[Agg[PathRef]] = T {
    resolveDeps(scalaDocIvyDeps)()
  }

  def finalScalaDocOptions: T[Seq[String]] = T {
    val uniDocClasspath = this.uniDocClassPath().map(_.path)
    val opts            = scalaDocOptions()
    val generateInkuire = this.generateInkuire()
    val externals       = scalaDocExternalMappings()

    Seq(
      "-classpath",
      uniDocClasspath.iterator.mkString(OS.`type` match {
        case OS.Windows => ";"
        case _          => ":"
      })
    ) ++
      (if (generateInkuire) Seq("-Ygenerate-inkuire") else Seq()) ++
      (if (externals.isEmpty) Seq()
       else Seq(s"-external-mappings:${externals.iterator.mkString(",")}")) ++
      opts
  }

  def scalaDocOptionsFile: T[PathRef] = T {
    val outFile = T.dest / "scaladoc-opts.txt"

    val args = finalScalaDocOptions() ++
      uniDocInputs().map(_.path.toNIO.toString())

    write(outFile, args.iterator.mkString("\n"))

    mill.PathRef(outFile)
  }

  def scaladocs: T[PathRef] = T {
    val outDir = T.dest / "javadocs"
    os.makeDir.all(outDir)

    val main              = scalaDocMainClass()
    val scalaDocClasspath = this.scalaDocClasspath()
    val optFile           = scalaDocOptionsFile().path

    val args = Seq(
      "-d",
      outDir.toNIO.toString(),
      s"@$optFile"
    )

    Jvm.inprocess(
      classPath = scalaDocClasspath.map(_.path),
      classLoaderOverrideSbtTesting = false,
      isolated = true,
      closeContextClassLoaderWhenDone = true,
      body = { cl =>
        val mainClass       = cl.loadClass(main)
        val mainMethod      = mainClass.getMethod("run", classOf[Array[String]])
        val mainContructor  = mainClass.getConstructor()
        val mainInstance    = mainContructor.newInstance()
        val reporter        = mainMethod.invoke(mainInstance, args.toArray)
        val hasErrorsMethod = reporter.getClass().getMethod("hasErrors")
        if (hasErrorsMethod.invoke(reporter).asInstanceOf[Boolean]) {
          T.log.error("ScalaDoc failed with errors")
          Result.Failure("UniDoc failed")
        } else {
          Result.Success(mill.PathRef(outDir))
        }
      }
    )
    /*
    val callResult =
      Jvm.callSubprocess(main, scalaDocClasspath.map(_.path), mainArgs = args)

    if (callResult.exitCode != 0)
      Result.Failure("ScalaDoc failed")
    else
      Result.Success(mill.PathRef(outDir))
     */
  }

  def open(): Command[Unit] = T.command {
    val docPath   = scaladocs().path
    val indexHtml = docPath / "index.html"

    val subproc = OS.`type` match {
      case OS.Linux =>
        Some(proc("xdg-open", indexHtml).call())
      case OS.Windows =>
        Some(proc("start", indexHtml).call())
      case _ =>
        None
    }

    subproc match {
      case Some(result) if result.exitCode == 0 => Result.Success(())
      case Some(_) => Result.Failure("Command failed")
      case None    => Result.Failure("Unsupported operating system")
    }
  }
}
