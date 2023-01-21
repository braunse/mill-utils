package de.sebbraun.millutils

import os.Shellable
import os.proc
import java.util.Locale
import java.awt.Window

object OS {
  sealed trait Type
  case object Windows extends Type
  case object Linux extends Type
  case object MacOS extends Type
  case object Other extends Type

  val `type`: Type = {
    val osName = System.getProperty("os.name").toLowerCase(Locale.ENGLISH)
    if(osName.contains("mac") || osName.contains("darwin")) MacOS
    else if(osName.contains("win")) Windows
    else if(osName.contains("nux")) Linux
    else Other
  }
}
