import scala.util.Properties
import mill.scalalib._
import mill.scalalib.publish._

def millVersionFile = T.source(PathRef(os.pwd / ".mill-version"))

def millVersion = T{
    os.read(millVersionFile().path).trim
}

object meta extends Module {
  def optString(s: String): Option[String] =
    if(s == null) None
    else {
      val st = s.trim()
      if(st.isEmpty()) None
      else Some(st)
    }

  def versionFromEnv = T.input { Properties.propOrNone("PUBLISH_VERSION") }
  def versionFromGitSha = T.input { optString(os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim) }
  def versionFromGitTag = T.input { optString(os.proc("git", "tag", "-l", "-n0", "--points-at", "HEAD").call().out.trim.stripPrefix("v")) }
  def publishVersion = T { (versionFromEnv() orElse versionFromGitTag() orElse versionFromGitSha()).getOrElse("latest") }
}

object utils extends ScalaModule with PublishModule {
    def scalaVersion = "2.13.10"

    def ivyDeps = Agg(
      ivy"com.lihaoyi::mill-scalalib:${millVersion()}",
      ivy"com.lihaoyi::mill-scalajslib:${millVersion()}"
    )

    def artifactName = "mill-utils"

    def publishVersion = meta.publishVersion()
    def pomSettings = PomSettings(
      description = "Utilities for Mill in my projects",
      organization = "com.github.braunse",
      url = "https://github.com/braunse/mill-utils",
      licenses = Seq(License.`MPL-2.0`),
      versionControl = VersionControl.github("braunse", "mill-utils"),
      developers = Seq(
        Developer(
          id = "braunse",
          name = "Sebastien Braun",
          url = "https://github.com/braunse"
        )
      )
    )
}
