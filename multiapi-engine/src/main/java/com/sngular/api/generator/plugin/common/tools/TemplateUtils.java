package com.sngular.api.generator.plugin.common.tools;

import org.apache.commons.text.CaseUtils;

public class TemplateUtils {

  public TemplateUtils() {
  }

  public String toLowerCamel(final String value) {
    if (value == null) {
      return null;
    }
    return CaseUtils.toCamelCase(value, false, '_');
  }

  public String toUpperCamel(final String value) {
    if (value == null) {
      return null;
    }
    return CaseUtils.toCamelCase(value, true, '_');
  }
}
