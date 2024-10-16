package com.sngular.api.generator.plugin.common.tools;

import org.apache.commons.text.CaseUtils;

public class StringCaseUtils {

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

    private StringCaseUtils() {
    }
}
