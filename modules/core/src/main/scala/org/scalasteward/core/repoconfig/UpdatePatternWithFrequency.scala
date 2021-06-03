/*
 * Copyright 2018-2021 Scala Steward contributors
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

package org.scalasteward.core.repoconfig

import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import org.scalasteward.core.data.{GroupId, Update}

final case class UpdatePatternWithFrequency(
    groupId: GroupId,
    artifactId: Option[String],
    version: Option[UpdatePattern.Version],
    frequency: PullRequestFrequency
)

object UpdatePatternWithFrequency {
  def findFrequencyOverride(
      patterns: List[UpdatePatternWithFrequency],
      update: Update.Single
  ): Option[PullRequestFrequency] = {
    val byGroupId = patterns.filter(_.groupId === update.groupId)
    val byArtifactId = byGroupId.filter(_.artifactId.forall(_ === update.artifactId.name))
    val dependencyFrequency = update.newerVersions.toList
      .flatMap(newVersion =>
        byArtifactId.find(_.version.forall(_.matches(newVersion))).map(_.frequency)
      )
      .headOption
    dependencyFrequency
  }

  implicit val updatePatternWithFrequencyDecoder: Decoder[UpdatePatternWithFrequency] =
    deriveDecoder

  implicit val updatePatternWithFrequencyEncoder: Encoder[UpdatePatternWithFrequency] =
    deriveEncoder
}
