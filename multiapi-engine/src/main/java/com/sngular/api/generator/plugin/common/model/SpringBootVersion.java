/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.model;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;

/**
 * The Spring Boot version the generated code targets, as configured with {@code springBootVersion}: {@code MAJOR} or
 * {@code MAJOR.MINOR}. A bare major means the lowest minor of that line ({@code 3} is 3.0), so a feature that needs a later
 * minor is only generated for projects that say they have it, and configurations written before minors were accepted keep
 * generating the same code.
 */
public final class SpringBootVersion {

  private static final Pattern VERSION = Pattern.compile("(\\d+)(?:\\.(\\d+))?(?:\\.\\d+)?");

  private final int major;

  private final int minor;

  private SpringBootVersion(final int major, final int minor) {
    this.major = major;
    this.minor = minor;
  }

  /**
   * Parses {@code MAJOR}, {@code MAJOR.MINOR} or {@code MAJOR.MINOR.PATCH} (the patch is ignored). Blank means the plugins'
   * default, 2.
   */
  public static SpringBootVersion parse(final String version) {
    final SpringBootVersion parsed;
    if (Objects.isNull(version) || version.isBlank()) {
      parsed = new SpringBootVersion(2, 0);
    } else {
      final Matcher matcher = VERSION.matcher(version.trim());
      if (!matcher.matches()) {
        throw new CodeGenerationException("springBootVersion must be MAJOR or MAJOR.MINOR, such as 3 or 3.2, but was '" + version + "'");
      }
      parsed = new SpringBootVersion(Integer.parseInt(matcher.group(1)), Objects.isNull(matcher.group(2)) ? 0 : Integer.parseInt(matcher.group(2)));
    }
    return parsed;
  }

  public static SpringBootVersion of(final Integer major) {
    return new SpringBootVersion(Objects.isNull(major) ? 2 : major, 0);
  }

  public int getMajor() {
    return major;
  }

  public boolean isAtLeast(final int otherMajor, final int otherMinor) {
    return major > otherMajor || major == otherMajor && minor >= otherMinor;
  }

  @Override
  public String toString() {
    return major + "." + minor;
  }
}
