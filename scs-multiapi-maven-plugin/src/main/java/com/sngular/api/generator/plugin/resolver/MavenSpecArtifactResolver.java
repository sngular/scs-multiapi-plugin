/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.resolver;

import java.io.File;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import com.sngular.api.generator.plugin.common.loader.SpecArtifactResolver;
import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.DependencyManagement;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.RepositorySystem;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.artifact.DefaultArtifact;
import org.eclipse.aether.repository.RemoteRepository;
import org.eclipse.aether.resolution.ArtifactRequest;
import org.eclipse.aether.resolution.ArtifactResolutionException;

/**
 * Resolves spec artifacts through Maven Resolver, which is what makes the repositories, mirrors,
 * proxies and credentials declared in {@code settings.xml} apply — a spec published to a private
 * repository is downloaded exactly like any other dependency.
 */
public class MavenSpecArtifactResolver implements SpecArtifactResolver {

  private static final String JAR_EXTENSION = "jar";

  private final RepositorySystem repositorySystem;

  private final RepositorySystemSession repositorySession;

  private final List<RemoteRepository> remoteRepositories;

  private final MavenProject project;

  public MavenSpecArtifactResolver(
      final RepositorySystem repositorySystem,
      final RepositorySystemSession repositorySession,
      final List<RemoteRepository> remoteRepositories,
      final MavenProject project) {
    this.repositorySystem = repositorySystem;
    this.repositorySession = repositorySession;
    this.remoteRepositories = remoteRepositories;
    this.project = project;
  }

  @Override
  public File resolveArtifact(final String groupId, final String artifactId, final String version) {
    final String resolvedVersion = StringUtils.isNotBlank(version) ? version : versionFromProject(groupId, artifactId);
    final var request = new ArtifactRequest(new DefaultArtifact(groupId, artifactId, JAR_EXTENSION, resolvedVersion), remoteRepositories, null);

    try {
      final File artifact = repositorySystem.resolveArtifact(repositorySession, request).getArtifact().getFile();
      if (Objects.isNull(artifact)) {
        throw new SpecDependencyException(String.format("Maven resolved %s:%s:%s to no file.", groupId, artifactId, resolvedVersion));
      }
      return artifact;
    } catch (final ArtifactResolutionException e) {
      throw new SpecDependencyException(String.format(
          "Cannot resolve %s:%s:%s. Check the coordinates and that the repository publishing it is reachable from this build.",
          groupId, artifactId, resolvedVersion), e);
    }
  }

  /**
   * {@code fromVersion} is optional: when it is omitted the version already declared by the module
   * is used, so the spec artifact stays pinned in a single place alongside the other dependencies.
   */
  private String versionFromProject(final String groupId, final String artifactId) {
    Optional<String> declared = declaredVersion(project.getDependencies(), groupId, artifactId);
    final DependencyManagement management = project.getDependencyManagement();
    if (declared.isEmpty() && Objects.nonNull(management)) {
      declared = declaredVersion(management.getDependencies(), groupId, artifactId);
    }
    return declared.orElseThrow(() -> new SpecDependencyException(String.format(
        "No version given for %s:%s. Set <fromVersion>, or declare the artifact as a dependency of this module.", groupId, artifactId)));
  }

  private Optional<String> declaredVersion(final List<Dependency> dependencies, final String groupId, final String artifactId) {
    return Optional.ofNullable(dependencies).orElse(List.of()).stream()
                   .filter(dependency -> groupId.equals(dependency.getGroupId()) && artifactId.equals(dependency.getArtifactId()))
                   .map(Dependency::getVersion)
                   .filter(StringUtils::isNotBlank)
                   .findFirst();
  }
}
