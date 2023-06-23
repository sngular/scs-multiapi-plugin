<#ftl output_format="plainText">
package ${packageModel};

import com.fasterxml.jackson.annotation.JsonValue;
<#assign field = schema.fieldObjectList[0]>

public enum ${schema.className} {
<#list field.enumValues as key, value>
  ${key}(${value})<#sep>,
</#list>;

  private ${field.dataType.innerType?cap_first} value;

  ${schema.className?cap_first}(${field.dataType.innerType?cap_first} value) {
    this.value = value;
  }

  @JsonValue
  public ${field.dataType.innerType?cap_first} getValue() {
    return value;
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }
}
