<#ftl output_format="plainText">
package ${packageModel};

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType == "enum">
import com.fasterxml.jackson.annotation.JsonValue;
    <#break>
  </#if>
</#list>
<#list schema.importList as import>
import ${import};
</#list>
import lombok.Builder;
import lombok.Data;
<#list schema.fieldObjectList as field>
  <#if field.required>
import lombok.NonNull;
    <#break>
  </#if>
</#list>
import lombok.extern.jackson.Jacksonized;
<#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import ${packageModel}.exception.ModelClassException;
</#if>

@Data
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName?uncap_first}")
  <#if field.required?has_content && field.required == true>
  @NonNull
  </#if>
  <#if field.dataType.baseType == "array">
  private ${field.dataType} ${field.baseName?uncap_first} = new ${field.dataType?api.getImplementationTypeString()}();
  <#elseif field.dataType.baseType == "map">
  private ${field.dataType} ${field.baseName?uncap_first} = new ${field.dataType?api.getImplementationTypeString()}();
  <#elseif field.dataType.baseType == "enum">
  private ${field.baseName?cap_first} ${field.baseName?uncap_first};

  public enum ${field.baseName?cap_first} {
<#list field.enumValues as key, value>
    ${key}(${value})<#sep>,
</#list>;

    private ${field.dataType.innerType?cap_first} value;

    ${field.baseName?cap_first}(${field.dataType.innerType?cap_first} value) {
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
  <#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">
  private ${field.dataType.innerType?cap_first} ${field.baseName?uncap_first};
  <#else>
  private ${field.dataType?cap_first} ${field.baseName?uncap_first};
  </#if>

</#list>

  @Builder
  @Jacksonized
  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
    <#if field.required>@NonNull </#if><#if field.dataType.baseType == "enum">${field.baseName?cap_first} ${field.baseName?uncap_first}<#elseif field.dataType.baseType == "array">${field.dataType} ${field.baseName?uncap_first}<#elseif field.dataType.baseType == "map">${field.dataType} ${field.baseName?uncap_first}<#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">${field.dataType.innerType?cap_first} ${field.baseName?uncap_first}<#else>${field.dataType?cap_first} ${field.baseName?uncap_first}</#if><#sep>, </#list>) {</@compress>
    <#list schema.fieldObjectList as field>
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
    </#list>

    <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
    validatePartialCombinations();
    </#if>
  }

  <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

  <#list schema.fieldObjectList as field>
    <#if field == schema.fieldObjectList?first>
    if (Objects.nonNull(this.${field.baseName?uncap_first})) {<#else> else if (Objects.nonNull(this.${field.baseName?uncap_first})) {</#if>
      satisfiedCondition = true;
    }</#list>

    if (!satisfiedCondition) {
      throw new ModelClassException("${schema.className}");
    }
  }
  </#if>
}
