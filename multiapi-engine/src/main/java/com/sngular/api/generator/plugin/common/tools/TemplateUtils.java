package com.sngular.api.generator.plugin.common.tools;

import org.apache.commons.text.CaseUtils;

public class TemplateUtils {

  public TemplateUtils() {
  }

  public String toLowerCamel(final String value) {
    if (value == null) {
      return null;
    }
    // If the value contains delimiters (underscore, hyphen or space), convert to camel case.
    // Otherwise, assume it's already in the desired camel-case style and return unchanged to preserve acronyms and casing.
    if (value.indexOf('_') >= 0 || value.indexOf('-') >= 0 || value.indexOf(' ') >= 0) {
      return CaseUtils.toCamelCase(value, false, '_', '-', ' ');
    }
    return value;
  }

  public String toUpperCamel(final String value) {
    if (value == null) {
      return null;
    }
    if (value.indexOf('_') >= 0 || value.indexOf('-') >= 0 || value.indexOf(' ') >= 0) {
      return CaseUtils.toCamelCase(value, true, '_', '-', ' ');
    }
    return value;
  }
}
