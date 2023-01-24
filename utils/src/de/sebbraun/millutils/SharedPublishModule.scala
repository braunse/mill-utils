package de.sebbraun.millutils

import mill._
import mill.scalalib._
import mill.scalalib.publish._
import mill.scalajslib._

trait SharedPublishModule extends SharedModule { outer =>
  def artifactName: T[String]
  def pomSettings: T[PomSettings]
  def publishVersion: T[String]

  override def moduleDeps: Seq[SharedPublishModule]       = Seq()
  override def backendModuleDeps: Seq[PublishModule]      = Seq()
  override def frontendModuleDeps: Seq[PublishModule]     = Seq()

  trait FrontendPublishModule extends FrontendModule with PublishModule {
    override def artifactName = T{ outer.artifactName() }
    def publishVersion = T{ outer.publishVersion() }
    def pomSettings = T{ outer.pomSettings() }
    override def moduleDeps: Seq[ScalaJSModule with PublishModule] = super.moduleDeps.map(_.asInstanceOf[ScalaJSModule with PublishModule])
  }

  trait BackendPublishModule extends PublishModule {
    override def artifactName = T{ outer.artifactName() }
    def publishVersion = T{ outer.publishVersion() }
    def pomSettings = T{ outer.pomSettings() }
    override def moduleDeps: Seq[ScalaModule with PublishModule] = super.moduleDeps.map(_.asInstanceOf[ScalaModule with PublishModule])
  }
}
