package org.scalasteward.core.mima

import org.scalasteward.core.data.Dependency
import org.scalasteward.core.mock.MockContext._
import org.scalasteward.core.mock.MockState
import org.scalatest.{FunSuite, Matchers}
import org.scalasteward.core.util.Nel

class MimaAlgTest extends FunSuite with Matchers {

  test("backwardBinaryCompatibility: Compatible") {
    val dep = Dependency("org.typelevel", "cats-effect", "cats-effect_2.12", "1.3.1")
    val update = dep.toUpdate.copy(newerVersions = Nel.of("1.4.0"))

    val (state, result) = mimaAlg
      .backwardBinaryCompatibily(update)
      .run(MockState.empty)
      .unsafeRunSync()

    result shouldBe BinaryCompatibility.Compatible
    state shouldBe MockState.empty.copy(
      commands = Vector(),
      logs = Vector(),
      files = Map()
    )
  }

  test("backwardBinaryCompatibility: Compatible for test dependencies even if incompatible") {
    val dep = Dependency(
      "org.scalatest",
      "scalatest",
      "scalatest_2.12",
      "3.0.5",
      configurations = Some("test")
    )
    val update = dep.toUpdate.copy(newerVersions = Nel.of("3.0.8"))

    val (state, result) = mimaAlg
      .backwardBinaryCompatibily(update)
      .run(MockState.empty)
      .unsafeRunSync()

    result shouldBe BinaryCompatibility.Compatible
    state shouldBe MockState.empty.copy(
      commands = Vector(),
      logs = Vector(),
      files = Map()
    )
  }

  test("backwardBinaryCompatibility: Incompatible") {
    val dep = Dependency("io.circe", "circe-core", "circe-core_2.12", "0.10.1")
    val update = dep.toUpdate.copy(newerVersions = Nel.of("0.11.1"))

    val (state, result) = mimaAlg
      .backwardBinaryCompatibily(update)
      .run(MockState.empty)
      .unsafeRunSync()

    result match {
      case BinaryCompatibility.Incompatible(lib, problems) =>
        problems.length shouldBe 7
        lib shouldBe Lib("io.circe", "circe-core", "0.10.1", "0.11.1")
      case _ =>
        fail(s"Incompatible expected but ${result}")
    }
    state shouldBe MockState.empty.copy(
      commands = Vector(),
      logs = Vector(),
      files = Map()
    )
  }

  test("backwardBinaryCompatibility: Err") {
    val dep2 = Dependency("fail", "fail", "fail", "1.3.1")
    val update2 = dep2.toUpdate.copy(newerVersions = Nel.of("1.4.0"))
    val result = mimaAlg
      .backwardBinaryCompatibily(update2)
      .runA(MockState.empty)
      .unsafeRunSync()
    result shouldBe an[BinaryCompatibility.Err]
  }
}
