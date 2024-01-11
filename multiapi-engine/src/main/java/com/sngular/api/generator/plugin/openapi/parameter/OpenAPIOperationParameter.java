package com.sngular.api.generator.plugin.openapi.parameter;

import com.sngular.api.generator.plugin.common.parameter.OperationParameter;
import java.util.Collections;
import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class OpenAPIOperationParameter extends OperationParameter {

  private String clientPackage;

  private boolean callMode;

  private boolean useTagsGroup;

  private boolean isReactive;

  @Override
  public Map<String, String> getFormats() {
    return Collections.emptyMap();
  }
}
