/*
 * Copyright 2018-2019 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.mima

import java.io.File

import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import cats.Parallel
import com.typesafe.tools.mima
import coursier._
import coursier.interop.cats._
import io.chrisdavenport.log4cats.Logger
import org.scalasteward.core.data.Update
import org.scalasteward.core.util._

import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

trait MimaAlg[F[_]] {
  def backwardBinaryCompatibily(update: Update): F[BinaryCompatibility]
}

object MimaAlg {
  def defaultLimit = Int.MaxValue

  def create[F[_]](
      implicit
      log: Logger[F],
      F: Sync[F]
  ): MimaAlg[F] = new MimaAlg[F] {
    implicit private val P = Parallel.identity[F]
    implicit private val cs: ContextShift[F] = new ContextShift[F] {
      override def shift: F[Unit] = F.unit
      override def evalOn[A](ec: ExecutionContext)(fa: F[A]): F[A] = F.defer(fa)
    }
    private val coursierCache = coursier.cache.FileCache[F]()
    private val fetchClient = coursier.Fetch[F](coursierCache)

    // To cache `backwardBinaryCompatibility` with `Async.memoize`,
    // memoized function need to be instantiated in `StewardAlg` and passed to `NurtureAlg`
    // since `MimaAlg.backwardBinaryCompatibility` could be invoked on same `Update` on different `Repo`s.
    // To avoid such complication, MimaAlg self-manages cache using `TrieMap`.
    private val mimaCache = new TrieMap[Lib, BinaryCompatibility]()

    override def backwardBinaryCompatibily(update: Update): F[BinaryCompatibility] =
      update match {
        case Update.Single(_, _, _, _, config) if config.contains("test") =>
          F.pure(BinaryCompatibility.Compatible)
        case _ =>
          val lib = Lib(
            update.groupId,
            update.artifactId,
            update.currentVersion,
            update.newerVersions.head
          )
          mimaCache.get(lib) match {
            case Some(value) => F.pure(value)
            case None        => backwardBinaryIssues0(lib)
          }
      }

    private[this] val repos = Seq(
      Repositories.central,
      Repositories.sbtPlugin("releases")
    )

    private def backwardBinaryIssues0(lib: Lib): F[BinaryCompatibility] = {

      def fetch0(version: String, artifactId: String, attributes: Map[String, String]): F[File] =
        fetchClient
          .addDependencies(
            Dependency.of(
              Module(Organization(lib.groupId), ModuleName(artifactId), attributes),
              version
            )
          )
          .withRepositories(repos)
          .io
          .flatMap { files =>
            F.fromTry(files.filter(_.getName === s"${artifactId}-${version}.jar") match {
              case Seq(jar) => Success(jar)
              case Seq()    => Failure(new Exception("Could not find jar"))
              case xs       => Failure(new Exception(s"Found multiple jars !? ${xs}"))
            })
          }

      def fetch(v: String): F[File] = {
        val attempts = Nel.of(
          F.defer(fetch0(v, lib.artifactId, Map.empty)),
          // TODO: Handle Scala version other than 2.12
          F.defer(fetch0(v, lib.artifactId + "_2.12", Map.empty)),
          F.defer(fetch0(v, lib.artifactId, Map("scalaVersion" -> "2.12", "sbtVersion" -> "1.0"))),
          F.defer(fetch0(v, lib.artifactId, Map("scalaVersion" -> "2.10", "sbtVersion" -> "0.13")))
        )
        attempts.reduceLeft { (left, right) =>
          left
            .onError {
              case e => log.info(s"fail: ${e.getMessage}")
            }
            .orElse(right)
        }
      }

      def checkBinaryCompatibility(oldDir: String, newDir: String): F[BinaryCompatibility] =
        F.fromTry(Try {
          val problems = newMimaInstance(log).collectProblems(oldDir, newDir)
          problems match {
            case Nil => BinaryCompatibility.Compatible
            case x :: xs =>
              BinaryCompatibility.Incompatible(lib, NonEmptyList.of(x, xs: _*).map(_.description("current")))
          }
        })

      val main = for {
        currentJar <- fetch(lib.current)
        newJar <- fetch(lib.newer)
        result <- checkBinaryCompatibility(currentJar.getAbsolutePath, newJar.getAbsolutePath)
      } yield mimaCache.getOrElseUpdate(lib, result)

      main.recoverWith {
        case e: Throwable =>
          for {
            _ <- log.warn(e)("Faild to execute mima.")
          } yield BinaryCompatibility.Err(e)
      }
    }
  }

  private def newMimaInstance[F[_]](log: Logger[F]): mima.lib.MiMaLib = {
    mima.core.Config.setup("scala-steward", Array.empty)
    val classpath = mima.core.reporterClassPath("")
    new mima.lib.MiMaLib(classpath, new MimaLogger(log))
  }

  private class MimaLogger[F[_]](log: Logger[F]) extends mima.core.util.log.Logging {
    override def debugLog(str: String): Unit = {
      val _ = log.debug(str)
    }
    override def error(str: String): Unit = {
      val _ = log.error(str)
    }
    override def info(str: String): Unit = {
      val _ = log.info(str)
    }
    override def warn(str: String): Unit = {
      val _ = log.warn(str)
    }
  }
}
