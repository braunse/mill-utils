package de.sebbraun.millutils

private[millutils] object NES {
  def unapply(s: String): Option[String] = {
    if (s != null && !s.isEmpty()) Some(s)
    else None
  }
}
