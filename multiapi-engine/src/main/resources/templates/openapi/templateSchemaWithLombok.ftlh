package ${packageModel};

import java.util.Objects;

import com.fasterxml.jackson.annotation.JsonProperty;
<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "enum">
import com.fasterxml.jackson.annotation.JsonValue;
    <#break>
  </#if>
</#list>
<#list schema.importList as import>
import ${import};
</#list>
<#list schema.fieldObjectList as field>
    <#if field.maxLength?has_content || field.minLength?has_content>
import com.sngular.multifileplugin.testapi.model.customvalidator.Size;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.maximum?has_content>
import com.sngular.multifileplugin.testapi.model.customvalidator.Max;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.minimum?has_content>
import com.sngular.multifileplugin.testapi.model.customvalidator.Min;
        <#break>
    </#if>
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
  <#if field.minimum?has_content && (!field.exclusiveMinimum?has_content || field.exclusiveMinimum == false)>
  @Min(minimum = ${field.minimum})
  </#if>
  <#if field.minimum?has_content && field.exclusiveMinimum?has_content && field.exclusiveMinimum == true>
  @Min(minimum = ${field.minimum}, exclusive = ${field.exclusiveMinimum?string})
  </#if>
  <#if field.maximum?has_content && (!field.exclusiveMaximum?has_content || field.exclusiveMaximum == false)>
  @Max(maximum = ${field.maximum})
  </#if>
  <#if field.maximum?has_content && field.exclusiveMaximum?has_content && field.exclusiveMaximum == true>
  @Max(maximum = ${field.maximum}, exclusive = ${field.exclusiveMaximum?string})
  </#if>
  <#if field.maxLength?has_content && field.minLength?has_content>
  @Size(min =${field.minLength}, max =${field.maxLength})
  <#elseif field.maxLength?has_content && !field.minLength?has_content>
  @Size(max =${field.maxLength})
  <#elseif !field.maxLength?has_content && field.minLength?has_content>
  @Size(min =${field.minLength})
  </#if>
  <#if field.required?has_content && field.required == true>
  @NonNull
  </#if>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  private List<${field.dataType}> ${field.baseName?uncap_first} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  private Map<String, ${field.dataType}> ${field.baseName?uncap_first} = new HashMap<String, ${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
  private ${field.baseName?cap_first} ${field.baseName?uncap_first};

  public enum ${field.baseName?cap_first} {
<#list field.enumValues as key, value>
    ${key}(${value?no_esc})<#sep>,
</#list>;

    private ${field.dataType?cap_first} value;

    ${field.baseName?cap_first}(${field.dataType?cap_first} value) {
      this.value = value;
    }

    @JsonValue
    public ${field.dataType?cap_first} getValue() {
      return value;
    }

    @Override
    public String toString() {
      return String.valueOf(value);
    }
  }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "Object">
  private ${field.dataType?cap_first} ${field.baseName?uncap_first};
  <#elseif field.dataTypeSimple?has_content>
  private ${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first};
  </#if>

</#list>

  @Builder
  @Jacksonized
  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
    <#if field.required>@NonNull </#if><#if field.dataType?has_content && field.dataTypeSimple == "enum">${field.baseName?cap_first} ${field.baseName?uncap_first}<#elseif field.dataType?has_content && field.dataTypeSimple == "array">List<${field.dataType}> ${field.baseName?uncap_first}<#elseif field.dataType?has_content && field.dataTypeSimple == "map">Map<String, ${field.dataType}> ${field.baseName?uncap_first}<#elseif field.dataType?has_content && field.dataTypeSimple == "Object">${field.dataType?cap_first} ${field.baseName?uncap_first}<#else>${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first}</#if><#sep>, </#list>) {</@compress>
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
