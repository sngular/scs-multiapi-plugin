package com.sngular.api.generator.plugin.common.parameter;

import com.sngular.api.generator.plugin.common.model.TimeType;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Builder.Default;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class OperationParameter {

  protected String filePath;

  protected String apiPackage;

  protected String modelPackage;

  protected String modelNameSuffix;

  protected String modelNamePrefix;

  protected String classNamePostfix;

  protected boolean useLombokModelAnnotation;

  @Default protected TimeType useTimeType = TimeType.LOCAL;

  public abstract Map<String, String> getFormats();
}