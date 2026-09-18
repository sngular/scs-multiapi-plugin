/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import java.util.Set;

import org.apache.commons.text.CaseUtils;

public class StringCaseUtils {

  private static final Set<String> JAVA_RESERVED_WORDS = Set.of(
      "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum",
      "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native",
      "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
      "throw", "throws", "transient", "true", "try", "void", "volatile", "while");

  private StringCaseUtils() {
  }

  public static String titleToSnakeCase(String titleCase) {
    StringBuilder sb = new StringBuilder();
    for (int i = 0; i < titleCase.length(); i++) {
      char c = titleCase.charAt(i);
      if (Character.isUpperCase(c)) {
        if (i > 0) {
          sb.append("_");
        }
        sb.append(c);
      } else {
        sb.append(Character.toUpperCase(c));
      }
    }
    return sb.toString();
  }

  public static String toCamelCase(final String toCamelCase) {
    return CaseUtils.toCamelCase(toCamelCase, true, '_');
  }

  /**
   * Turns a name taken from a contract into a name the generated Java code can use to build identifiers with.
   *
   * <p>Names that already are legal Java identifiers are returned untouched, so the generated code keeps using the name the contract chose. Any other name
   * - a header such as {@code Idempotency-Key}, a property such as {@code client.ref} - is rebuilt in lower camel case out of its alphanumeric runs, and
   * prefixed with {@code _} when the result would start with a digit. The contract name is not replaced anywhere it travels over the wire; only the
   * identifier is adapted.</p>
   *
   * <p>A reserved word is returned as it is: it still names a perfectly good type ({@code New}) or accessor ({@code getNew()}), and the templates prefix it
   * where it is a variable. Use {@link #toJavaVariableName(String)} to get a name that can be declared as a variable on its own.</p>
   */
  public static String toJavaIdentifier(final String name) {
    if (name == null || name.isEmpty()) {
      return name;
    }
    if (isValidJavaIdentifier(name)) {
      return name;
    }
    final StringBuilder identifier = new StringBuilder();
    boolean capitalizeNext = false;
    for (int i = 0; i < name.length(); i++) {
      final char current = name.charAt(i);
      if (Character.isLetterOrDigit(current)) {
        if (identifier.isEmpty()) {
          identifier.append(Character.toLowerCase(current));
        } else {
          identifier.append(capitalizeNext ? Character.toUpperCase(current) : current);
        }
        capitalizeNext = false;
      } else {
        capitalizeNext = !identifier.isEmpty();
      }
    }
    if (identifier.isEmpty()) {
      return "_" + Integer.toHexString(name.hashCode());
    }
    if (!Character.isJavaIdentifierStart(identifier.charAt(0))) {
      identifier.insert(0, '_');
    }
    return identifier.toString();
  }

  /**
   * As {@link #toJavaIdentifier(String)}, and additionally prefixes with {@code _} a name that is a Java reserved word, so that the result can be declared
   * as a variable: a parameter named {@code new} is declared as {@code _new}.
   */
  public static String toJavaVariableName(final String name) {
    final String identifier = toJavaIdentifier(name);
    return JAVA_RESERVED_WORDS.contains(identifier) ? "_" + identifier : identifier;
  }

  private static boolean isValidJavaIdentifier(final String name) {
    if (!Character.isJavaIdentifierStart(name.charAt(0))) {
      return false;
    }
    for (int i = 1; i < name.length(); i++) {
      if (!Character.isJavaIdentifierPart(name.charAt(i))) {
        return false;
      }
    }
    return true;
  }
}
