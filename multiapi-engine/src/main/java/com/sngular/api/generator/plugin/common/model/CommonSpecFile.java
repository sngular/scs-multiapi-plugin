package com.sngular.api.generator.plugin.common.model;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
public class CommonSpecFile implements ExternalSpecSource {

  private String filePath;

  private String apiPackage;

  private String modelPackage;

  private String modelNamePrefix;

  private String modelNameSuffix;

  private String classNamePostfix;

  private boolean useLombokModelAnnotation;

  private boolean generateSpringwolfAnnotations;

  private boolean usePactAnnotation;

  /**
   * Whether every generated enum gets an {@code UNKNOWN} constant that reading any value outside the contract resolves to, instead of
   * failing, and that is written back as {@code "UNKNOWN"}. Unset means {@code true}; {@code false} generates strict enums that reject
   * unknown values. A contract that already declares an {@code UNKNOWN} value keeps it and gets no extra constant.
   */
  private Boolean useUnknownEnumValue;

  @Builder.Default
  private String dateTimeFormat = "yyyy-MM-dd'T'HH:mm:ss";

  @Builder.Default
  private String dateFormat = "yyyy-MM-dd";

  @Builder.Default
  private TypeConstants.TimeType useTimeType = TypeConstants.TimeType.LOCAL;

  /**
   * Coordinates of the artifact that publishes the contract. When set, {@link #filePath} is read
   * from inside that artifact instead of from the module's filesystem. See {@link ExternalSpecSource}.
   */
  private String fromGroupId;

  private String fromArtifactId;

  private String fromVersion;

  public boolean shouldUseUnknownEnumValue() {
    return !Boolean.FALSE.equals(useUnknownEnumValue);
  }

  public Map<String, String> getFormats() {
    return Map.of("DATE_TIME", dateTimeFormat, "DATE", dateFormat);
  }
}
