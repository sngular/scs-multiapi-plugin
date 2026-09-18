/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.model;

import java.util.List;

/**
 * The conventional location of a contract, used when a spec file declares no {@code filePath}.
 *
 * <p>A project that follows it configures nothing: {@code contract/openapi.yml} for the OpenAPI
 * goal and {@code contract/asyncapi.yml} for the AsyncAPI one, whether the contract lives in the
 * module or inside a published artifact. The {@code .yaml} spelling is accepted as well, because
 * failing over a file extension helps nobody.</p>
 */
public final class SpecConventions {

  /** Folder holding the contract, relative to the module or to the root of the artifact. */
  public static final String CONTRACT_FOLDER = "contract";

  private static final List<String> EXTENSIONS = List.of(".yml", ".yaml");

  private SpecConventions() {
  }

  /**
   * @param rootMarker {@code "openapi"} or {@code "asyncapi"}, the kind of contract being generated.
   * @return the conventional paths to try, in order of preference.
   */
  public static List<String> defaultFilePaths(final String rootMarker) {
    return EXTENSIONS.stream().map(extension -> CONTRACT_FOLDER + "/" + rootMarker + extension).toList();
  }

  /**
   * @return the conventional path to name in messages, the preferred spelling of
   *     {@link #defaultFilePaths(String)}.
   */
  public static String defaultFilePath(final String rootMarker) {
    return defaultFilePaths(rootMarker).get(0);
  }
}
