/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.loader;

import java.io.File;

/**
 * Resolves the artifact that holds an API contract to a local file.
 *
 * <p>The engine is build-tool agnostic, so it never talks to a repository itself: the Maven plugin
 * supplies an implementation backed by Maven Resolver (honouring {@code settings.xml}, mirrors,
 * private repositories and credentials) and the Gradle plugin one backed by a detached
 * configuration. {@link LocalRepositorySpecArtifactResolver} is the fallback used when the engine
 * runs without a build tool.</p>
 */
@FunctionalInterface
public interface SpecArtifactResolver {

  /**
   * @param groupId    artifact groupId, never blank.
   * @param artifactId artifact artifactId, never blank.
   * @param version    artifact version, or {@code null}/blank to let the implementation infer it
   *                   from the versions already declared by the consuming build.
   * @return the resolved artifact file, downloading it if the build tool deems it necessary.
   */
  File resolveArtifact(String groupId, String artifactId, String version);
}
