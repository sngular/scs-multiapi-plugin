<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]>
<#function calculateSafeName propertyName ender>
    <#if words?seq_contains(propertyName?uncap_first)>
        <#return "_"+propertyName+ender/>
    <#else >
        <#return propertyName+ender/>
    </#if>
</#function>
<#function singularizeName propertyName>
    <#if propertyName?matches(".*s")>
        <#return propertyName?remove_ending("s")/>
    <#elseif propertyName?matches(".*List")>
        <#return propertyName?remove_ending("List")/>
    <#else >
        <#return propertyName/>
    </#if>
</#function>
package ${packageModel};

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "enum">
import com.fasterxml.jackson.annotation.JsonValue;
    <#break>
  </#if>
</#list>
import io.swagger.v3.oas.annotations.media.Schema;
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
    <#if field.required || schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import ${exceptionPackage}.exception.ModelClassException;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.pattern?has_content>
import ${packageModel}.customvalidator.Pattern;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.multipleOf?has_content>
import ${packageModel}.customvalidator.MultipleOf;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.required?has_content && field.required == true>
import ${packageModel}.customvalidator.NotNull;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true>
import ${packageModel}.customvalidator.UniqueItems;
        <#break>
    </#if>
</#list>

@JsonDeserialize(builder = ${schema.className}.${schema.className}Builder.class)
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
  @NotNull
</#if>
  <#if field.dataType?has_content && (field.dataTypeSimple == "array" || field.dataTypeSimple == "Array")>
    <#if field.required>
  private final List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, ";")}
    <#else>
  private List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")} = new ArrayList<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}>();
    </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    <#if field.required>
  private final Map<String, ${field.dataType}> ${calculateSafeName (field.baseName, ";")}
    <#else>
  private Map<String, <#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")} = new HashMap<String, <#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}>();
    </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    <#if field.required>
  private final ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
    <#else>
  private ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
    </#if>
  public enum ${field.baseName?cap_first} {
<#list field.enumValues as value>
    ${value?upper_case}("${value?no_esc}")<#sep>,
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
    <#if field.required>
      <#if field.parentPackage??>
  private final ${packageModel}.${field.parentPackage}.${field.dataTypeSimple} ${calculateSafeName (field.baseName, ";")}
      <#else>
  private final ${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, ";")}
      </#if>
      <#elseif field.parentPackage??>
  private ${packageModel}.${field.parentPackage}.${field.dataTypeSimple} ${calculateSafeName (field.baseName, ";")}
      <#else>
  private ${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, ";")}
    </#if>
  </#if>
</#list>

  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
    <#if field.dataType?has_content && field.dataTypeSimple == "enum">${field.baseName?cap_first} ${calculateSafeName (field.baseName, "")}<#elseif field.dataType?has_content && field.dataTypeSimple == "array">List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")}<#elseif field.dataType?has_content && field.dataTypeSimple == "map">Map<String, ${field.dataType}> ${calculateSafeName (field.baseName, "")}<#else><#if field.parentPackage??>${packageModel}.${field.parentPackage}.${field.dataTypeSimple?cap_first}<#else>${field.dataTypeSimple?cap_first}</#if> ${calculateSafeName (field.baseName, "")}</#if><#sep>, </#list>) {</@compress>
    <#list schema.fieldObjectList as field>
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
    </#list>

    <#list schema.fieldObjectList as field>
      <#if field.required>
    validateRequiredAttributes();
      <#break>
      </#if>
    </#list>
    <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
    validatePartialCombinations();
    </#if>
  }

  private ${schema.className}(${schema.className}Builder builder) {
  <#list schema.fieldObjectList as field>
    this.${calculateSafeName (field.baseName, "")} = builder.${calculateSafeName (field.baseName, ";")}
  </#list>

  <#list schema.fieldObjectList as field>
    <#if field.required>
    validateRequiredAttributes();
    <#break>
    </#if>
  </#list>
  <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
    validatePartialCombinations();
  </#if>
  }

  public static ${schema.className}.${schema.className}Builder builder() {
    return new ${schema.className}.${schema.className}Builder();
  }

  @JsonPOJOBuilder(buildMethodName = "build", withPrefix = "")
  public static class ${schema.className}Builder {

<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
    private List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")} = new ArrayList<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    private Map<String, <#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")} = new HashMap<String,<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    private ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
  <#else>
    private <#if field.parentPackage??>${packageModel}.${field.parentPackage}.${field.dataTypeSimple?cap_first}<#else>${field.dataTypeSimple?cap_first}</#if> ${calculateSafeName (field.baseName, ";")}
  </#if>
</#list>

<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
    <#assign propName>${calculateSafeName (field.baseName, "")}</#assign>
    public ${schema.className}.${schema.className}Builder ${propName}(List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${propName}) {
      if (!${propName}.isEmpty()) {
        this.${propName}.addAll(${propName});
      }
      return this;
    }

    <#assign singName>${singularizeName (calculateSafeName (field.baseName, ""))}</#assign>
    public ${schema.className}.${schema.className}Builder ${singName}(<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType} ${singName}) {
      if (${singName} != null) {
        this.${propName}.add(${singName});
      }
      return this;
    }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    <#assign propName>${calculateSafeName (field.baseName, "")}</#assign>
    public ${schema.className}.${schema.className}Builder ${propName}(Map<String, ${field.dataType}> ${propName}) {
      this.${propName} =  ${propName};
      return this;
    }

    public ${schema.className}.${schema.className}Builder ${propName}Value(String key, ${field.dataType} value) {
      this.${propName}.put(key, value);
      return this;
    }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${field.baseName?cap_first} ${calculateSafeName (field.baseName, "")}) {
      this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
      return this;
    }
  <#else>
    <#if field.parentPackage??>
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${packageModel}.${field.parentPackage}.${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, "")}) {
    <#else>
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, "")}) {
    </#if>
      this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, "")};
      return this;
    }
  </#if>

  </#list>
    public ${schema.className} build() {
      ${schema.className} ${schema.className?uncap_first} = new ${schema.className}(this);
      return ${schema.className?uncap_first};
    }
  }

<#list schema.fieldObjectList as field>
  /**
  * Get ${field.baseName?uncap_first}
  * @return ${field.baseName?uncap_first}
  */
  @Schema(name = "${field.baseName?uncap_first}", required = <#if field.required?has_content && field.required == true>true<#else>false</#if>)
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  public List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(List<<#if field.parentPackage??>${packageModel}.${field.parentPackage}.</#if>${field.dataType}> ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  public Map<String, ${field.dataType}> get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(Map<String, ${field.dataType}> ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
  public ${field.baseName?cap_first} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(${field.baseName?cap_first} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataTypeSimple?has_content>
  <#if field.parentPackage??>
  public ${packageModel}.${field.parentPackage}.${field.dataTypeSimple?cap_first} get${field.baseName?cap_first}() {
  <#else>
  public ${field.dataTypeSimple?cap_first} get${field.baseName?cap_first}() {
  </#if>
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if field.required == false>
    <#if field.parentPackage??>
  public void set${field.baseName?cap_first}(${packageModel}.${field.parentPackage}.${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first}) {
    <#else>
  public void set${field.baseName?cap_first}(${field.dataTypeSimple?cap_first} ${calculateSafeName (field.baseName, "")}) {
    </#if>
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, "")};
  }
  </#if>
  </#if>

</#list>
  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    ${schema.className} ${schema.className?uncap_first} = (${schema.className}) o;
    return <#compress><#list schema.fieldObjectList as field> Objects.equals(this.${calculateSafeName (field.baseName, "")}, ${schema.className?uncap_first}.${calculateSafeName (field.baseName, "")})<#if field?has_next> && </#if></#list>;</#compress>
  }

  @Override
  public int hashCode() {
    return Objects.hash(<#list schema.fieldObjectList as field>${calculateSafeName (field.baseName, "")}<#if field?has_next>, </#if></#list>);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("${schema.className}{");
<#list schema.fieldObjectList as field>
  <#if field_has_next>
    sb.append(" ${field.baseName?uncap_first}:").append(${calculateSafeName (field.baseName, "")}).append(",");
  <#else>
    sb.append(" ${field.baseName?uncap_first}:").append(${calculateSafeName (field.baseName, "")});
  </#if>
</#list>
    sb.append("}");
    return sb.toString();
  }

  <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

  <#list schema.fieldObjectList as field>
    <#if field == schema.fieldObjectList?first>
    if (Objects.nonNull(this.${calculateSafeName (field.baseName, "")})) {
    <#else> else if (Objects.nonNull(this.${calculateSafeName (field.baseName, "")})) {
    </#if>
      satisfiedCondition = true;
    }</#list>

    if (!satisfiedCondition) {
      throw new ModelClassException("${schema.className}");
    }
  }
  </#if>

  <#list schema.fieldObjectList as field>
  <#if field.required>
  private void validateRequiredAttributes() {
    boolean satisfiedCondition = true;

  <#list schema.fieldObjectList?filter(f -> f.required) as field>
    <#if field?index == 0>
    if (!Objects.nonNull(this.${calculateSafeName (field.baseName, "")})) {
    <#else>
    else if (!Objects.nonNull(this.${calculateSafeName (field.baseName, "")})) {
    </#if>
      satisfiedCondition = false;
    }</#list>

    if (!satisfiedCondition) {
      throw new ModelClassException("${schema.className}");
    }
  }
<#break>
</#if>
</#list>
}
