# Repository-specific configuration

You can add a configuration file named either `.scala-steward.conf` or `scala-steward.conf` to configure how Scala Steward updates your repository.
The `[.]scala-steward.conf` configuration file can be located in the root of your repository, in `.github` directory or in `.config` directory (searched in this order).
If a configuration file exists in more than one location, only the first found file is taken into account.

```scala mdoc:passthrough
import io.circe.refined._
import io.circe.syntax._
import org.scalasteward.core.repoconfig._

print("```properties")
print(s"""
# pullRequests.frequency allows to control how often or when Scala Steward
# is allowed to create pull requests.
#
# Possible values:
#   @asap
#     PRs are created without delay.
#
#   <timespan>
#     PRs are created only again after the given timespan since the last PR
#     has passed. Example values are "36 hours", "1 day", or "14 days".

#   <CRON expression>
#     PRs are created roughly according to the given CRON expression.
#
#     CRON expressions consist of five fields:
#     minutes, hour of day, day of month, month, and day of week.
#
#     See https://www.alonsodomin.me/cron4s/userguide/index.html#parsing for
#     more information about the CRON expressions that are supported.
#
#     Note that the date parts of the CRON expression are matched exactly
#     while the time parts are only used to abide to the frequency of
#     the given expression.
#
# Default: ${PullRequestsConfig.defaultFrequency.asJson.noSpaces}
#
#pullRequests.frequency = "0 0 ? * 3" # every thursday on midnight
pullRequests.frequency = "7 days"

# pullRequests.grouping allows you to specify how Scala Steward should group
# your updates in order to reduce the number of pull-requests.
#
# Updates will be placed in the first group with which they match, starting
# from the first in the array. Those that do not match any group will follow
# the default procedure (one PR per update).
#
# Each element in the array will have the following schema:
#
#   - name (mandatory): the name of the group, will be used for things like naming the branch
#   - title (optional): if provided it will be used as the title for the PR
#   - filter (mandatory): a non-empty list containing the filters to use to know
#                         if an update falls into this group.
#
# `filter` properties would have this format:
#
#    {
#       version = "major" | "minor" | "patch" | "pre-release" | "build-metadata",
#       group = "{group}",
#       artifact = "{artifact}"
#    }
#
# For more information on the values for the `version` filter visit https://semver.org/
#
# Every field in a `filter` is optional but at least one must be provided.
#
# For grouping every update together a filter like {group = "*"} can be # provided.
#
# To create a new PR for each unique combination of artifact-versions, include $${hash} in the name.
#
# Default: []
pullRequests.grouping = [
  { name = "patches", "title" = "Patch updates", "filter" = [{"version" = "patch"}] },
  { name = "minor_major", "title" = "Minor/major updates", "filter" = [{"version" = "minor"}, {"version" = "major"}] },
  { name = "typelevel", "title" = "Typelevel updates", "filter" = [{"group" = "org.typelevel"}, {"group" = "org.http4s"}] },
  { name = "my_libraries_$${hash}", "filter" = [{"artifact" = "my-library"}, {"artifact" = "my-other-library", "group" = "my-org"}] },
  { name = "all", "title" = "Dependency updates", "filter" = [{"group" = "*"}] }
]

# pullRequests.includeMatchedLabels allows to control which autogenerated labels are added to PRs
# This setting has no effect on custom labels (see below).
# via a regex check each label is checked against.
# Defaults to no regex (all labels are added) which is equivalent to ".*".
pullRequests.includeMatchedLabels = "(.*semver.*)|(commit-count:n:.*)"

# pullRequests.customLabels allows to add custom labels to PRs.
# This is useful if you want to use the labels for automation (project board for example).
# Defaults to no labels (no labels are added).
pullRequests.customLabels = [ "dependencies", "scala" ]

# Only these dependencies which match the given patterns are updated.
#
# Each pattern must have `groupId`, and may have `artifactId` and `version`.
# Defaults to empty `[]` which mean Scala Steward will update all dependencies.
updates.allow  = [ { groupId = "com.example" } ]

# The dependencies which match the given version pattern are updated.
# Dependencies that are not listed will be updated.
#
# Each pattern must have `groupId`, `version` and optional `artifactId`.
# Defaults to empty `[]` which mean Scala Steward will update all dependencies.
# the following example will allow to update foo when version is 1.1.x
updates.pin  = [ { groupId = "com.example", artifactId="foo", version = "1.1." } ]

# The dependencies which match the given pattern are NOT updated.
#
# Each pattern must have `groupId`, and may have `artifactId` and `version`.
# Defaults to empty `[]` which mean Scala Steward will not ignore dependencies.
updates.ignore = [ { groupId = "org.acme", artifactId="foo", version = "1.0" } ]

# The dependencies which match the given patterns are allowed to be updated to pre-release from stable.
# This also implies, that it will be allowed for snapshot versions to be updated to snapshots of different series.
#
# Each pattern must have `groupId`, and may have `artifactId` and `version`.
# Defaults to empty `[]` which mean Scala Steward will ignore all stable to pre-release update options.
updates.allowPreReleases  = [ { groupId = "com.example", artifactId="foo" } ]

# If set, Scala Steward will only create or update `n` PRs each time it runs (see `pullRequests.frequency` above).
# Useful if running frequently and/or CI build are costly
# Default: ${UpdatesConfig.defaultLimit.asJson.noSpaces}
updates.limit = 5

# The extensions of files that should be updated.
# Default: ${UpdatesConfig.defaultFileExtensions.toList.sorted.asJson.noSpaces}
updates.fileExtensions = [".scala", ".sbt", ".sbt.shared", ".sc", ".yml", ".md", ".markdown", ".txt"]

# If "on-conflicts", Scala Steward will update the PR it created to resolve conflicts as
# long as you don't change it yourself.
# If "always", Scala Steward will always update the PR it created as long as
# you don't change it yourself.
# If "never", Scala Steward will never update the PR
# Default: ${PullRequestUpdateStrategy.default.asJson.noSpaces}
updatePullRequests = "always" | "on-conflicts" | "never"

# If set, Scala Steward will use this message template for the commit messages and PR titles.
# Supported variables: $${artifactName}, $${currentVersion}, $${nextVersion} and $${default}
# Default: ${CommitsConfig.defaultMessage.asJson.noSpaces} which is equivalent to "Update $${artifactName} to $${nextVersion}"
commits.message = "Update $${artifactName} from $${currentVersion} to $${nextVersion}"

# If true and when upgrading version in .scalafmt.conf, Scala Steward will perform scalafmt
# and add a separate commit when format changed. So you don't need reformat manually and can merge PR.
# If false, Scala Steward will not perform scalafmt, so your CI may abort when reformat needed.
# Default: ${ScalafmtConfig.defaultRunAfterUpgrading.asJson.noSpaces}
scalafmt.runAfterUpgrading = false

# It is possible to have multiple scala projects in a single repository. In that case the folders containing the projects (build.sbt folders)
# are specified using the buildRoots property. Note that the paths used there are relative and if the repo directory itself also contains a build.sbt the dot can be used to specify it.
# Default: ${RepoConfig.defaultBuildRoots.asJson.noSpaces}
buildRoots = [ ".", "subfolder/projectA" ]

# Define commands that are executed after an update via a hook.
# A groupId and/or artifactId can be defined to only execute after certain dependencies are updated. If neither is defined, the hook runs for every update.
postUpdateHooks = [{
  command = ["sbt", "protobufGenerate"],
  commitMessage = "Regenerated protobuf files",
  groupId = "com.github.sbt",
  artifactId = "sbt-protobuf"
}]

# You can override some config options for dependencies that matches the given pattern.
# Currently, "pullRequests" can be overridden.
# Each pattern must have `groupId`, and may have `artifactId` and `version`.
# First-matched entry is used.
# More-specific entry should be placed before less-specific entry.
#
# Default: empty `[]`
dependencyOverrides = [
  {
    dependency = { groupId = "com.example", artifactId = "foo", version = "2." },
    pullRequests = { frequency = "1 day" },
  },
  {
    dependency = { groupId = "com.example", artifactId = "foo" },
    pullRequests = { frequency = "30 day" },
  },
  {
    dependency = { groupId = "com.example" },
    pullRequests = { frequency = "14 day" },
  }
]

# Assign people from the list to the pull request or request a review.
# Currently supported only for GitLab and GitHub.
# GitLab users - free version of GitLab only supports one assignee and one reviewer, others will be ignored.
# GitHub users - to request review from a team inside your organisation it should be specified
# like "yourOrg/yourTeam" in `reviewers` config below.
# Please note that only accounts with write access to the repository (Developer role for GitLab) are able
# to add assignees or request reviews. Consequently, it won't work for public @scala-steward instance on GitHub.
assignees = [ "username1", "username2" ]
reviewers = [ "username1", "username2" ]

# If true, Scala Steward will sign off all commits (e.g. `git --signoff`).
# Default: false
signoffCommits = true
""")
println("```")
```

The version information given in the patterns above can be in two formats:
1. just a `version` field that is treated as a prefix of the version
2. a structure consisting of any of the following fields:
   * `prefix`: is matched against the beginning of the version
   * `suffix`: is matched against the end of the version
   * `exact`: is matched against the whole version
   * `contains`: is matched against substrings in the version

```properties
version = "1.1."
version = { prefix = "1.1." }
version = { suffix = "jre8" }
version = { prefix = "1.1.", suffix = "jre8" }
version = { exact = "1.1.2.jre8" }
version = { contains = "feature" }
```

## Ignore lines

Though `updates.ignores` offers granular configuration to exclude dependencies from update, Scala Steward also recognizes markers in file to ignore lines.

Dependencies in lines between `// scala-steward:off` and `// scala-steward:on` are not updated.

```scala
libraryDependencies ++= Seq(
  // scala-steward:off
  "com.github.pathikrit" %% "better-files" % "3.8.0",
  "com.olegpy" %% "better-monadic-for" % "0.3.1",
  // scala-steward:on
  "org.typelevel" %% "cats-effect" % "1.3.1",  // This and subsequent will get updated
  "org.typelevel" %% "cats-kernel-laws" % "1.6.1"
)
```

Also, the line ends with `// scala-steward:off` is not updated solely.

```scala
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.0", // scala-steward:off
  "com.typesafe.akka" %% "akka-testkit" % "2.5.0", // This and subsequent will get updated
)
```