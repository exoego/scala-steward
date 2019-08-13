package org.scalasteward.core.mima

import cats.data.NonEmptyList

sealed abstract class BinaryCompatibility

object BinaryCompatibility {
  case object Compatible extends BinaryCompatibility

  final case class Incompatible(lib: Lib, problems: NonEmptyList[String])
      extends BinaryCompatibility {

    def summary: String =
      s"""Found ${problems.size} potential binary incompatibilities in `"${lib.groupId}" % "${lib.artifactId}" % "${lib.current}" => "${lib.newer}"`."""
  }

  final case class Err(err: Throwable) extends BinaryCompatibility
}
