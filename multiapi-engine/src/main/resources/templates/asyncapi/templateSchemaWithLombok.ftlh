<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]>
<#function calculateSafeName propertyName ender>
    <#if words?seq_contains(propertyName?uncap_first)>
        <#return "_"+propertyName+ender/>
    <#else >
        <#return propertyName+ender/>
    </#if>
</#function>
<#function requireFinal property>
  <#if property.required || property.const?has_content>
      <#return "final "/>
  <#else >
      <#return ""/>
  </#if>
</#function>
<#function hasConstValue value>
    <#if value?has_content>
        <#return "= " + value +";"/>
    <#else >
        <#return ";"/>
    </#if>
</#function>
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
    <#if field.restrictions.maxLength?has_content || field.restrictions.minLength?has_content>
import ${packageModel}.customvalidator.Size;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.maximum?has_content>
import ${packageModel}.customvalidator.Max;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.minimum?has_content>
import ${packageModel}.customvalidator.Min;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.pattern?has_content>
import ${packageModel}.customvalidator.Pattern;
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.multipleOf?has_content>
import ${packageModel}.customvalidator.MultipleOf;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.maxItems?has_content>
import ${packageModel}.customvalidator.MaxItems;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.minItems?has_content>
import ${packageModel}.customvalidator.MinItems;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true>
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
<#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import ${exceptionPackage}.exception.ModelClassException;
</#if>

@Data
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName?uncap_first}")
  <#if field.restrictions.minimum?has_content && (!field.restrictions.exclusiveMinimum?has_content || field.restrictions.exclusiveMinimum == false)>
  @Min(minimum = "${field.restrictions.minimum}", exclusive = false)
  </#if>
  <#if field.restrictions.minimum?has_content && field.restrictions.exclusiveMinimum?has_content && field.restrictions.exclusiveMinimum == true>
  @Min(minimum = "${field.restrictions.minimum}", exclusive = ${field.restrictions.exclusiveMinimum?string})
  </#if>
  <#if field.restrictions.maximum?has_content && (!field.restrictions.exclusiveMaximum?has_content || field.restrictions.exclusiveMaximum == false)>
  @Max(maximum = "${field.restrictions.maximum}", exclusive = false)
  </#if>
  <#if field.restrictions.maximum?has_content && field.restrictions.exclusiveMaximum?has_content && field.restrictions.exclusiveMaximum == true>
  @Max(maximum = "${field.restrictions.maximum}", exclusive = ${field.restrictions.exclusiveMaximum?string})
  </#if>
  <#if field.restrictions.maxItems?has_content>
  @MaxItems(maximum = ${field.restrictions.maxItems})
  </#if>
  <#if field.restrictions.minItems?has_content>
  @MinItems(minimum = ${field.restrictions.minItems})
  </#if>
  <#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true>
  @UniqueItems
  </#if>
  <#if field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(min =${field.restrictions.minLength}, max =${field.restrictions.maxLength})
  <#elseif field.restrictions.maxLength?has_content && !field.restrictions.minLength?has_content>
  @Size(max =${field.restrictions.maxLength})
  <#elseif !field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(min =${field.restrictions.minLength})
  </#if>
  <#if field.restrictions.pattern?has_content>
  @Pattern(regex = "${field.restrictions.pattern}")
  </#if>
  <#if field.restrictions.multipleOf?has_content>
  @MultipleOf(multiple = "${field.restrictions.multipleOf}")
  </#if>
  <#if field.required?has_content && field.required == true>
  @NonNull
  </#if>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  @Builder.Default
  private List<${field.dataType}> ${calculateSafeName (field.baseName, "")} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  @Builder.Default
  private Map<String, ${field.dataType}> ${calculateSafeName (field.baseName, "")} = new HashMap<String, ${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
  private ${requireFinal (field)}${field.baseName?cap_first} ${calculateSafeName (field.baseName, hasConstValue(field.constValue))}

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
  <#elseif field.dataTypeSimple?has_content>
  private ${requireFinal (field)}${field.baseName?cap_first} ${calculateSafeName (field.baseName, hasConstValue(field.constValue))}
  </#if>

</#list>

  @Builder
  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
    <#if field.required>@NonNull </#if><#if field.dataType?has_content && field.dataTypeSimple == "enum">${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}<#elseif field.dataType?has_content && field.dataTypeSimple == "array">List<${field.dataType}> ${calculateSafeName (field.baseName, ";")}<#elseif field.dataType?has_content && field.dataTypeSimple == "map">Map<String, ${field.dataType}> ${calculateSafeName (field.baseName, ";")}<#else>${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, ";")}</#if><#sep>, </#list>) {</@compress>
    <#list schema.fieldObjectList as field>
    this.${calculateSafeName (field.baseName, ";")} = ${calculateSafeName (field.baseName, ";")};
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
    if (Objects.nonNull(this.${calculateSafeName (field.baseName, ";")})) {
    <#else>
    else if (Objects.nonNull(this.${calculateSafeName (field.baseName, ";")})) {
    </#if>
      satisfiedCondition = true;
    }
  </#list>

    if (!satisfiedCondition) {
      throw new ModelClassException("${schema.className}");
    }
  }
  </#if>
}
