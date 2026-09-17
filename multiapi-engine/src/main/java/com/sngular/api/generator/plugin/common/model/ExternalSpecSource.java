/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.model;

import com.sngular.api.generator.plugin.exception.SpecDependencyException;
import org.apache.commons.lang3.StringUtils;

/**
 * Implemented by every spec file configuration that can declare its contract as living inside a
 * published artifact instead of the module's own sources.
 *
 * <p>When {@link #usesExternalDependency()} is {@code true}, {@code filePath} is no longer a
 * filesystem path relative to the module: it is the path of the contract <em>inside</em> the
 * resolved artifact.</p>
 */
public interface ExternalSpecSource {

  String getFilePath();

  String getFromGroupId();

  String getFromArtifactId();

  String getFromVersion();

  /**
   * @return {@code true} when the contract must be read from an external artifact rather than from
   *     the filesystem or the plugin classpath.
   */
  default boolean usesExternalDependency() {
    return StringUtils.isNotBlank(getFromGroupId()) && StringUtils.isNotBlank(getFromArtifactId());
  }

  /**
   * Rejects half-declared coordinates instead of silently falling back to filesystem resolution,
   * which is what made a typo in either field surface as a confusing "file not found".
   */
  default void validateDependencyCoordinates() {
    final boolean hasGroupId = StringUtils.isNotBlank(getFromGroupId());
    final boolean hasArtifactId = StringUtils.isNotBlank(getFromArtifactId());
    if (hasGroupId != hasArtifactId) {
      throw new SpecDependencyException(String.format(
          "Incomplete dependency coordinates for spec '%s': fromGroupId and fromArtifactId must both be set (got fromGroupId='%s', fromArtifactId='%s').",
          getFilePath(), StringUtils.defaultString(getFromGroupId()), StringUtils.defaultString(getFromArtifactId())));
    }
  }

  /**
   * @return the coordinates in {@code groupId:artifactId[:version]} form, for logs and error
   *     messages. The version is left out when it was not configured, because it is then whatever
   *     the consuming build already declares.
   */
  default String getDependencyCoordinate() {
    return StringUtils.isBlank(getFromVersion())
        ? String.format("%s:%s", getFromGroupId(), getFromArtifactId())
        : String.format("%s:%s:%s", getFromGroupId(), getFromArtifactId(), getFromVersion());
  }
}
