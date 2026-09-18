package com.sngular.api.generator.plugin.common.model;

import java.util.Map;

import com.sngular.api.generator.plugin.common.tools.StringCaseUtils;

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

  private String description;

  private String example;

  private boolean deprecated;

  /**
   * The name under which this property is declared in the generated Java code. It is the name the contract uses whenever that name is a legal Java
   * identifier, and a sanitized version of it otherwise - a property named {@code client-ref} is declared as {@code clientRef}. The contract name stays in
   * {@link #baseName} and remains the name the property is serialized under.
   */
  public String getFieldName() {
    return StringCaseUtils.toJavaIdentifier(baseName);
  }
}
