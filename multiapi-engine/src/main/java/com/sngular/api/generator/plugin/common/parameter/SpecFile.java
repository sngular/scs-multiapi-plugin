package com.sngular.api.generator.plugin.common.parameter;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class SpecFile {

  protected String filePath;

  private String modelNamePrefix;

  private String modelNameSuffix;
}
