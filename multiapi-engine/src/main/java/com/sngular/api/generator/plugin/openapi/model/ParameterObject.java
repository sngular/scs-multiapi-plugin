/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.List;
import java.util.Objects;

import com.sngular.api.generator.plugin.common.model.SchemaFieldObjectType;
import com.sngular.api.generator.plugin.common.model.TypeConstants;
import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ParameterObject {

  private static final List<String> DATE_TYPES = List.of(TypeConstants.LOCALDATE, TypeConstants.ZONEDDATE, TypeConstants.OFFSETDATE);

  private static final List<String> DATE_TIME_TYPES = List.of(TypeConstants.LOCALDATETIME, TypeConstants.ZONEDDATETIME, TypeConstants.OFFSETDATETIME);

  private String name;

  private String in;

  private Boolean required;

  private String description;

  private Boolean isCollection;

  private SchemaFieldObjectType dataType;

  private String importName;

  /** The contract's {@code schema.default}, as the text the binding annotation's {@code defaultValue} takes, or null. */
  private String defaultValue;

  /** Whether the Java name is always rebuilt in camel case ({@code useCamelCaseNames}), not only when the contract name is not a legal identifier. */
  private boolean camelCaseName;

  /** The contract's {@code style}, or null when it declares none. */
  private String style;

  /** The contract's {@code explode}, or null when it declares none. */
  private Boolean explode;

  /**
   * The serialization style: the declared one or, as OpenAPI defines, {@code form} for query and cookie parameters and
   * {@code simple} for path and header ones.
   */
  public String getEffectiveStyle() {
    final String effectiveStyle;
    if (Objects.nonNull(style)) {
      effectiveStyle = style;
    } else {
      effectiveStyle = "query".equals(in) || "cookie".equals(in) ? "form" : "simple";
    }
    return effectiveStyle;
  }

  /** Whether the value is exploded: the declared {@code explode} or, as OpenAPI defines, {@code true} only for the {@code form} style. */
  public boolean isEffectiveExplode() {
    return Objects.nonNull(explode) ? explode : "form".equals(getEffectiveStyle());
  }

  /**
   * The generated client's {@code CollectionFormat} for an array value, as its style and explode declare: exploded
   * {@code form} (the default) repeats the parameter ({@code ids=1&ids=2}), {@code form} without explode joins with commas,
   * {@code spaceDelimited} with spaces and {@code pipeDelimited} with pipes.
   */
  public String getCollectionFormat() {
    final String format;
    switch (getEffectiveStyle()) {
      case "spaceDelimited":
        format = "SSV";
        break;
      case "pipeDelimited":
        format = "PIPES";
        break;
      case "form":
        format = isEffectiveExplode() ? "MULTI" : "CSV";
        break;
      default:
        format = "CSV";
        break;
    }
    return format;
  }

  /**
   * The name under which this parameter is declared in the generated Java code. It is the contract name whenever that name is a legal Java identifier, and a
   * sanitized version of it otherwise - a header named {@code Idempotency-Key} is declared as {@code idempotencyKey}. The contract name stays in {@link #name}
   * and is what the generated code sends and binds against.
   */
  public String getVariableName() {
    return camelCaseName ? StringCaseUtils.toCamelCaseVariableName(name) : StringCaseUtils.toJavaVariableName(name);
  }

  /**
   * The {@code DateTimeFormat.ISO} constant a date parameter binds with ({@code DATE} or {@code DATE_TIME}), or null for any
   * other type. Spring does not parse ISO dates such as {@code 2024-01-31} from a request without it, although that is the
   * format {@code format: date} / {@code date-time} prescribe.
   */
  public String getDateTimeFormatIso() {
    String iso = null;
    if (Objects.nonNull(dataType)) {
      if (DATE_TYPES.stream().anyMatch(dataType::containsType)) {
        iso = "DATE";
      } else if (DATE_TIME_TYPES.stream().anyMatch(dataType::containsType)) {
        iso = "DATE_TIME";
      }
    }
    return iso;
  }
}
