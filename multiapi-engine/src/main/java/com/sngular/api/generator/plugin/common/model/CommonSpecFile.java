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

  public Map<String, String> getFormats() {
    return Map.of("DATE_TIME", dateTimeFormat, "DATE", dateFormat);
  }
}
