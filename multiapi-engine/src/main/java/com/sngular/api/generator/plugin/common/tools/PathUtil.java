/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import java.nio.file.InvalidPathException;
import java.nio.file.Paths;

import org.apache.commons.lang3.StringUtils;

/**
 * Utility class for path operations.
 */
public final class PathUtil {

  private PathUtil() {
    // Utility class
  }

  /**
   * Checks if a file path is absolute. An absolute path is platform-dependent: - On Windows: C:\path, D:\path, \\server\share (UNC) - On Unix/Linux: /path
   *
   * @param filePath the file path to check
   * @return true if the path is absolute, false if relative or invalid
   */
  public static boolean isAbsolutePath(final String filePath) {
    if (StringUtils.isEmpty(filePath)) {
      return false;
    }
    try {
      return Paths.get(filePath).isAbsolute();
    } catch (final InvalidPathException e) {
      // If the path is invalid, treat it as not absolute
      return false;
    }
  }
}
