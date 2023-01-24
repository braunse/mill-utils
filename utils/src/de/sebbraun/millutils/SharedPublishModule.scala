package de.sebbraun.millutils

import mill._
import mill.scalalib._
import mill.scalalib.publish._

trait SharedPublishModule extends Module {
  outer: SharedModule =>

  def artifactName: T[String]
  def pomSettings: T[PomSettings]
  def publishVersion: T[String]

  trait FrontendPublishModule extends PublishModule {
    override def artifactName = T{ outer.artifactName() }
    def publishVersion = T{ outer.publishVersion() }
    def pomSettings = T{ outer.pomSettings() }
  }

  trait BackendPublishModule extends PublishModule {
    override def artifactName = T{ outer.artifactName() }
    def publishVersion = T{ outer.publishVersion() }
    def pomSettings = T{ outer.pomSettings() }
  }
}
