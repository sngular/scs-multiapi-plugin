package com.sngular.api.generator.plugin.common.tools;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.StringUtils;

public class StringCaseUtils {

  private StringCaseUtils() {
  }

  public static String titleToSnakeCase(String titleCase) {
    if (StringUtils.isBlank(titleCase)) return titleCase;
    final StringBuilder sb = new StringBuilder();
    for (int i = 0; i < titleCase.length(); i++) {
      char c = titleCase.charAt(i);
      if (Character.isUpperCase(c)) {
        if (i > 0 && sb.charAt(sb.length() - 1) != '_') {
          sb.append('_');
        }
        sb.append(Character.toLowerCase(c));
      } else if (c == '-' || c == ' ' || c == '/') {
        if (sb.length() == 0 || sb.charAt(sb.length() - 1) != '_') sb.append('_');
      } else if (c == '_') {
        if (sb.length() == 0 || sb.charAt(sb.length() - 1) != '_') sb.append('_');
      } else {
        sb.append(c);
      }
    }
    // Return uppercase SNAKE_CASE to keep previous behaviour compatible with map keys
    return StringUtils.upperCase(sb.toString());
  }

  public static String toCamelCase(final String toCamelCase) {
    if (StringUtils.isBlank(toCamelCase)) return toCamelCase;
    // Known acronyms that should be uppercased when found as whole parts
    final Set<String> ACRONYMS = Stream.of("id", "ids", "dto", "dtos", "url", "uuid")
                                      .collect(Collectors.toSet());

    // Special combinators that should be converted into two words with internal capitalization
    final Set<String> COMBINATORS = Stream.of("allof", "anyof", "oneof").collect(Collectors.toSet());

    // Split on underscore, hyphen or space
    final String[] parts = toCamelCase.split("[_\\- ]+");
    final StringBuilder sb = new StringBuilder();
    for (String p : parts) {
      if (StringUtils.isBlank(p)) continue;
      final String lower = p.toLowerCase();
      // Handle combinators (allof/anyof/oneof) and acronyms based on lowercase token first
      if (COMBINATORS.contains(lower)) {
        sb.append(Character.toUpperCase(lower.charAt(0))).append(lower.substring(1, 3)).append(Character.toUpperCase(lower.charAt(3))).append(lower.substring(4));
        continue;
      }
      if (ACRONYMS.contains(lower)) {
        sb.append(lower.toUpperCase());
        continue;
      }
      // If the part already contains uppercase letters (camelCase/PascalCase inside), preserve them
      boolean hasUpperInside = p.chars().anyMatch(Character::isUpperCase);
      if (hasUpperInside) {
        // If it's ALL UPPER and not a known acronym, treat as normal word (capitalize only first letter)
        if (p.equals(p.toUpperCase()) && !ACRONYMS.contains(lower)) {
          sb.append(Character.toUpperCase(lower.charAt(0)));
          if (lower.length() > 1) sb.append(lower.substring(1));
          continue;
        }
        // Mixed case: preserve existing casing (only ensure first char is uppercase)
        sb.append(Character.toUpperCase(p.charAt(0))).append(p.substring(1));
        continue;
      }
      // Default: capitalize first letter
      sb.append(Character.toUpperCase(lower.charAt(0)));
      if (lower.length() > 1) sb.append(lower.substring(1));
    }
    return sb.toString();
  }
}
