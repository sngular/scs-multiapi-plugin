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
    <#if field.restrictionProperties.maxLength?has_content || field.restrictionProperties.minLength?has_content>
import ${packageModel}.customvalidator.Size;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.maximum?has_content>
import ${packageModel}.customvalidator.Max;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.minimum?has_content>
import ${packageModel}.customvalidator.Min;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.pattern?has_content>
import ${packageModel}.customvalidator.Pattern;
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.multipleOf?has_content>
import ${packageModel}.customvalidator.MultipleOf;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.maxItems?has_content>
import ${packageModel}.customvalidator.MaxItems;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.minItems?has_content>
import ${packageModel}.customvalidator.MinItems;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictionProperties.uniqueItems?has_content && field.restrictionProperties.uniqueItems == true>
import ${packageModel}.customvalidator.UniqueItems;
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
  <#if field.restrictionProperties.minimum?has_content && (!field.restrictionProperties.exclusiveMinimum?has_content || field.restrictionProperties.exclusiveMinimum == false)>
  @Min(minimum = "${field.restrictionProperties.minimum}", exclusive = false)
  </#if>
  <#if field.restrictionProperties.minimum?has_content && field.restrictionProperties.exclusiveMinimum?has_content && field.restrictionProperties.exclusiveMinimum == true>
  @Min(minimum = "${field.restrictionProperties.minimum}", exclusive = ${field.restrictionProperties.exclusiveMinimum?string})
  </#if>
  <#if field.restrictionProperties.maximum?has_content && (!field.restrictionProperties.exclusiveMaximum?has_content || field.restrictionProperties.exclusiveMaximum == false)>
  @Max(maximum = "${field.restrictionProperties.maximum}", exclusive = false)
  </#if>
  <#if field.restrictionProperties.maximum?has_content && field.restrictionProperties.exclusiveMaximum?has_content && field.restrictionProperties.exclusiveMaximum == true>
  @Max(maximum = "${field.restrictionProperties.maximum}", exclusive = ${field.restrictionProperties.exclusiveMaximum?string})
  </#if>
  <#if field.restrictionProperties.maxItems?has_content>
  @MaxItems(maximum = ${field.restrictionProperties.maxItems})
  </#if>
  <#if field.restrictionProperties.minItems?has_content>
  @MinItems(minimum = ${field.restrictionProperties.minItems})
  </#if>
  <#if field.restrictionProperties.uniqueItems?has_content && field.restrictionProperties.uniqueItems == true>
  @UniqueItems
  </#if>
  <#if field.restrictionProperties.maxLength?has_content && field.restrictionProperties.minLength?has_content>
  @Size(min =${field.restrictionProperties.minLength}, max =${field.restrictionProperties.maxLength})
  <#elseif field.restrictionProperties.maxLength?has_content && !field.restrictionProperties.minLength?has_content>
  @Size(max =${field.restrictionProperties.maxLength})
  <#elseif !field.restrictionProperties.maxLength?has_content && field.restrictionProperties.minLength?has_content>
  @Size(min =${field.restrictionProperties.minLength})
  </#if>
  <#if field.restrictionProperties.pattern?has_content>
  @Pattern(regex = "${field.restrictionProperties.pattern}")
  </#if>
  <#if field.restrictionProperties.multipleOf?has_content>
  @MultipleOf(multiple = "${field.restrictionProperties.multipleOf}")
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
