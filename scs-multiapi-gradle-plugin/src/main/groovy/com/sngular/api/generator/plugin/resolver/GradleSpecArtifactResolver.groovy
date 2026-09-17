/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.resolver

import com.sngular.api.generator.plugin.common.loader.SpecArtifactResolver
import com.sngular.api.generator.plugin.exception.SpecDependencyException
import org.gradle.api.Project

/**
 * Resolves spec artifacts through a detached configuration, so the repositories declared by the
 * build — including private ones with their credentials — are the ones used to fetch the contract.
 */
class GradleSpecArtifactResolver implements SpecArtifactResolver {

  private final Project project

  GradleSpecArtifactResolver(final Project project) {
    this.project = project
  }

  @Override
  File resolveArtifact(final String groupId, final String artifactId, final String version) {
    final String resolvedVersion = version ?: versionFromProject(groupId, artifactId)
    // '@jar' keeps this to the artifact itself: a spec artifact has no dependencies worth walking.
    final String notation = "${groupId}:${artifactId}:${resolvedVersion}@jar"
    try {
      final def dependency = project.dependencies.create(notation)
      return project.configurations.detachedConfiguration(dependency).resolve().first()
    } catch (final Exception e) {
      throw new SpecDependencyException(
          "Cannot resolve ${groupId}:${artifactId}:${resolvedVersion}. Check the coordinates and that the repository publishing it is declared in this build.", e)
    }
  }

  /**
   * fromVersion is optional: when it is omitted the version already declared by the build is used,
   * so the spec artifact stays pinned in a single place alongside the other dependencies.
   */
  private String versionFromProject(final String groupId, final String artifactId) {
    final String declared = project.configurations.collectMany { configuration -> configuration.dependencies }
                                   .find { dependency -> dependency.group == groupId && dependency.name == artifactId && dependency.version }
                                   ?.version
    if (!declared) {
      throw new SpecDependencyException(
          "No version given for ${groupId}:${artifactId}. Set fromVersion, or declare the artifact as a dependency of this project.")
    }
    return declared
  }
}
