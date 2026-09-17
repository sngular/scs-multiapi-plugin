/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;

import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import org.apache.commons.lang3.StringUtils;

/**
 * Fallback resolver that only inspects the local Maven repository. It never downloads anything, so
 * the build-tool backed resolvers are always preferred; this one exists for the engine's own tests
 * and for embedders that drive the generator without a build tool.
 */
public class LocalRepositorySpecArtifactResolver implements SpecArtifactResolver {

  private final Path localRepository;

  public LocalRepositorySpecArtifactResolver() {
    this(defaultLocalRepository());
  }

  public LocalRepositorySpecArtifactResolver(final Path localRepository) {
    this.localRepository = localRepository;
  }

  private static Path defaultLocalRepository() {
    final String configured = System.getProperty("maven.repo.local");
    return StringUtils.isNotBlank(configured)
        ? Paths.get(configured)
        : Paths.get(System.getProperty("user.home"), ".m2", "repository");
  }

  @Override
  public File resolveArtifact(final String groupId, final String artifactId, final String version) {
    if (StringUtils.isBlank(version)) {
      throw new SpecDependencyException(String.format(
          "No version available for %s:%s. Set fromVersion, or declare the artifact as a dependency of the module so the build can infer it.",
          groupId, artifactId));
    }

    Path artifactPath = localRepository;
    for (final String segment : groupId.split("\\.")) {
      artifactPath = artifactPath.resolve(segment);
    }
    final File artifact = artifactPath.resolve(artifactId).resolve(version).resolve(artifactId + "-" + version + ".jar").toFile();

    if (!artifact.isFile()) {
      throw new SpecDependencyException(String.format(
          "Cannot resolve %s:%s:%s from the local repository (looked for %s).", groupId, artifactId, version, artifact.getAbsolutePath()));
    }
    return artifact;
  }
}
