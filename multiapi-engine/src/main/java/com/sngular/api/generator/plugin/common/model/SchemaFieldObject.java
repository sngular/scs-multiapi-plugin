package com.sngular.api.generator.plugin.common.model;

import java.util.Map;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@Builder
@AllArgsConstructor
@EqualsAndHashCode(of = "baseName")
public class SchemaFieldObject {

  private String baseName;

  @Builder.Default
  private SchemaFieldObjectType dataType = new SchemaFieldObjectType(TypeConstants.OBJECT);

  @Builder.Default
  private SchemaFieldObjectProperties restrictions = new SchemaFieldObjectProperties();

  private String importClass;

  private boolean required;

  private Map<String, String> enumValues;

  private Object constValue;
}
