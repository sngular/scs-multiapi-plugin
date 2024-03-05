package com.sngular.api.generator.plugin.common.parameter;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public abstract class AbstractSpecFile {

  private String modelNamePostFix;

  private String modelNameSuffix;

  private String filePath;
}
