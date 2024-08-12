<#ftl output_format="plainText">
<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]>
<#function calculateSafeName propertyName ender const="">
    <#if words?seq_contains(propertyName?uncap_first) || propertyName?matches("[0-9].*")>
        <#if const?has_content>
          <#return "_"+propertyName + " = " + const + ender/>
        <#else>
          <#return "_"+propertyName + ender/>
        </#if>
    <#else >
      <#if const?has_content>
        <#return propertyName + " = " + const + ender/>
      <#else>
        <#return propertyName + ender/>
      </#if>
    </#if>
</#function>
<#function createSimpleType field>
  <#if (field.constValue?has_content)>
    ${calculateSafeName (field.baseName, ";", field.constValue)}
  <#else >
   ${calculateSafeName (field.baseName, ";", "")}
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
<#function requireFinal field>
    <#if field.required == true || field.constValue??>
      <#return "final"/>
    <#else >
        <#return ""/>
    </#if>
</#function>
<#function checkIfShouldSet field>
    <#if field.required == true || field.constValue??>
        <#return false/>
    <#else >
        <#return true/>
    </#if>
</#function>
<#function hasConstValue value="">
    <#if value?has_content>
        <#return " = " + renderValue (value) + ";"/>
    <#else >
        <#return ";"/>
    </#if>
</#function>
<#function renderValue value="">
  <#if value?is_boolean>
    <#return value == true>
  <#elseif value?is_number>
    <#return value>
  <#elseif value?is_string>
    <#return '\"${value}\"'>
  </#if>
</#function>
package ${packageModel};

import java.util.Objects;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.fasterxml.jackson.databind.annotation.JsonPOJOBuilder;
import com.fasterxml.jackson.annotation.JsonProperty;
<#list schema.fieldObjectList as field>
  <#if field.enumValues?has_content>
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
    <#if field.restrictions.format?has_content>
import com.fasterxml.jackson.annotation.JsonFormat;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
    <#if field.required ||  schema.schemaCombinator?has_content && (schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf")>
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
  @JsonProperty(value ="${calculateSafeName (field.baseName, "")}")
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
  <#if field.restrictions.format?has_content>
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "${field.restrictions.format}")
  </#if>
  <#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true>
  @UniqueItems
  </#if>
  <#if field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(min =${field.restrictions.minLength}, max =${field.restrictions.maxLength})
  <#elseif field.restrictions.maxLength?has_content && !field.restrictions.minLength?has_content>
  @Size(max =${field.restrictions.maxLength}, min =0)
  <#elseif !field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(max =0, min =${field.restrictions.minLength})
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
  <#if field.dataType.baseType == "array">
    <#if field.required>
  private final ${field.dataType} ${calculateSafeName (field.baseName, ";")}
    <#else>
  private ${field.dataType} ${calculateSafeName (field.baseName, "")} = new ${field.dataType?api.getImplementationTypeString()}();
    </#if>
  <#elseif field.dataType.baseType == "map">
    <#if field.required>
  private final ${field.dataType} ${calculateSafeName (field.baseName, ";")}
    <#else>
  private ${field.dataType} ${calculateSafeName (field.baseName, "")} = new ${field.dataType?api.getImplementationTypeString()}();
    </#if>
  <#elseif field.dataType.baseType == "enum">
    <#if field.required>
  private final ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
    <#else>
  private ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
    </#if>
  public enum ${field.baseName?cap_first} {
<#list field.enumValues as key, value>
  <#if key?matches("[0-9].*")>
    ${"_"+key}(${value})<#sep>,
  <#else>
    ${key}(${value})<#sep>,
  </#if>
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
    <#if field.required>
  private final ${field.dataType.innerType?cap_first} ${calculateSafeName (field.baseName, ";")}
    <#else>
  private ${field.dataType.innerType?cap_first} ${calculateSafeName (field.baseName, ";")}
    </#if>
  <#else>
  private ${requireFinal (field)} ${field.dataType} ${createSimpleType (field)}
  </#if>
</#list>

  private ${schema.className}(${schema.className}Builder builder) {
  <#list schema.fieldObjectList as field>
    <#if checkIfShouldSet(field)>
      this.${calculateSafeName (field.baseName, "")} = builder.${calculateSafeName (field.baseName, ";")}
    </#if>
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
  <#if field.dataType.baseType == "array">
    private ${field.dataType} ${calculateSafeName (field.baseName, "")} = new ${field.dataType?api.getImplementationTypeString()}();
  <#elseif field.dataType.baseType == "map">
    private ${field.dataType} ${calculateSafeName (field.baseName, "")} = new ${field.dataType?api.getImplementationTypeString()}();
  <#elseif field.dataType.baseType == "enum">
    private ${field.baseName?cap_first} ${calculateSafeName (field.baseName, ";")}
  <#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">
    private ${field.dataType.innerType?cap_first} ${calculateSafeName (field.baseName, ";")}
  <#elseif checkIfShouldSet(field)>
    private ${field.dataType} ${calculateSafeName (field.baseName, ";")}
  </#if>
</#list>
<#list schema.fieldObjectList as field>
  <#if field.dataType.baseType == "array">
    <#assign propName>${calculateSafeName (field.baseName, "")}</#assign>
    public ${schema.className}.${schema.className}Builder ${propName}(${field.dataType} ${propName}) {
      if (!${propName}.isEmpty()) {
        this.${propName}.addAll(${propName});
      }
      return this;
    }

    <#assign singName>${singularizeName (calculateSafeName (field.baseName, ""))}</#assign>
    public ${schema.className}.${schema.className}Builder ${singName}(${field.dataType.innerType} ${singName}) {
      if (${singName} != null) {
        this.${propName}.add(${singName});
      }
      return this;
    }
  <#elseif field.dataType.baseType == "map">
    <#assign propName>${calculateSafeName (field.baseName, "")}</#assign>
    public ${schema.className}.${schema.className}Builder ${propName}(${field.dataType} ${propName}) {
      this.${propName} = ${propName};
      return this;
    }

    public ${schema.className}.${schema.className}Builder ${propName}Value(String key, ${field.dataType.innerType} value) {
      this.${propName}.put(key, value);
      return this;
    }
  <#elseif field.dataType.baseType == "enum">
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${field.baseName?cap_first} ${field.baseName}) {
      this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
      return this;
    }
  <#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${field.dataType.innerType?cap_first} ${calculateSafeName (field.baseName, "")}) {
      this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
      return this;
    }
  <#elseif checkIfShouldSet(field)>
    public ${schema.className}.${schema.className}Builder ${calculateSafeName (field.baseName, "")}(${field.dataType} ${calculateSafeName (field.baseName, "")}) {
      this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
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
  @Schema(name = "${field.baseName?uncap_first}", required = <#if field.required?has_content && field.required == true>true<#else>false</#if>)
  <#if field.dataType.baseType == "array">
  public ${field.dataType} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if checkIfShouldSet (field)>
  public void set${field.baseName?cap_first}(${field.dataType} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataType.baseType == "map">
  public ${field.dataType} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if checkIfShouldSet (field)>
  public void set${field.baseName?cap_first}(${field.dataType} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataType.baseType == "enum">
  public ${field.baseName?cap_first} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if checkIfShouldSet (field)>
  public void set${field.baseName?cap_first}(${field.baseName?cap_first} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">
  public ${field.dataType.innerType?cap_first} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if checkIfShouldSet (field)>
  public void set${field.baseName?cap_first}(${field.dataType.innerType?cap_first} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
  }
  </#if>
  <#else>
  public ${field.dataType} get${field.baseName?cap_first}() {
    return ${calculateSafeName (field.baseName, ";")}
  }
  <#if checkIfShouldSet (field)>
  public void set${field.baseName?cap_first}(${field.dataType} ${calculateSafeName (field.baseName, "")}) {
    this.${calculateSafeName (field.baseName, "")} = ${calculateSafeName (field.baseName, ";")}
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
    </#if>  satisfiedCondition = true;
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
