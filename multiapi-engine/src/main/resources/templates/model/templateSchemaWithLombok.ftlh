<#ftl output_format="plainText">
<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]>
<#function calculateSafeName propertyName ender const="">
    <#if words?seq_contains(propertyName?uncap_first) || propertyName?matches("[0-9].*")>
        <#if const?has_content>
            <#return "_"+propertyName?uncap_first + " = " + const + ender/>
        <#else>
            <#return "_"+propertyName?uncap_first + ender/>
        </#if>
    <#else >
        <#if const?has_content>
            <#return propertyName?uncap_first + " = " + const + ender/>
        <#else>
            <#return propertyName?uncap_first + ender/>
        </#if>
    </#if>
</#function>
<#function createSimpleType field>
    <#if (field.constValue?has_content)>
        ${calculateSafeName (field.baseName, ";", field.const)}
    <#else >
        ${calculateSafeName (field.baseName, ";", "")}
    </#if>
</#function>
<#function singularizeName propertyName>
    <#if propertyName?matches(".*ies")>
        <#return propertyName?remove_ending("ies")+"y"/>
    <#elseif propertyName?matches(".*s")>
        <#return propertyName?remove_ending("s")/>
    <#elseif propertyName?matches(".*List")>
        <#return propertyName?remove_ending("List")/>
    <#else >
        <#return "_"+propertyName/>
    </#if>
</#function>
<#function calculateMaxAnnotation dataType>
    <#if dataType.baseType?lower_case == "bigdecimal">
        <#return "@MaxBigDecimal" >
    <#elseif dataType.baseType?lower_case == "integer">
        <#return "@MaxInteger" >
    <#elseif dataType.baseType?lower_case == "float">
        <#return "@MaxFloat" >
    <#elseif dataType.baseType?lower_case == "double">
        <#return "@MaxDouble" >
    </#if>
</#function>
<#function calculateMinAnnotation dataType>
    <#if dataType.baseType?lower_case == "bigdecimal">
        <#return "@MinBigDecimal" >
    <#elseif dataType.baseType?lower_case == "integer">
        <#return "@MinInteger" >
    <#elseif dataType.baseType?lower_case == "float">
        <#return "@MinFloat" >
    <#elseif dataType.baseType?lower_case == "double">
        <#return "@MinDouble" >
    </#if>
</#function>
package ${packageModel};

<#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import java.util.Objects;
</#if>
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
<#assign visited = "">
<#list schema.fieldObjectList as field>
<#if (field.restrictions.maxLength?has_content || field.restrictions.minLength?has_content) && !(visited?contains("size"))>
    <#assign visited += "size," />
import ${packageModel}.customvalidator.Size;
</#if>
<#if field.restrictions.maximum?has_content>
    <#if field.dataType.baseType?lower_case == "bigdecimal" && !(visited?contains("maxBigDecimal"))>
        <#assign visited += "maxBigDecimal," />
import ${packageModel}.customvalidator.MaxBigDecimal;
    <#elseif field.dataType.baseType?lower_case == "integer" &&  !(visited?contains("maxInteger"))>
        <#assign visited += "maxInteger," />
import ${packageModel}.customvalidator.MaxInteger;
    <#elseif field.dataType.baseType?lower_case == "float" &&  !(visited?contains("maxFloat"))>
        <#assign visited += "maxFloat," />
import ${packageModel}.customvalidator.MaxFloat;
    <#elseif field.dataType.baseType?lower_case == "double" &&  !(visited?contains("maxDouble"))>
        <#assign visited += "maxDouble," />
import ${packageModel}.customvalidator.MaxDouble;
    </#if>
</#if>
    <#if  field.restrictions.minimum?has_content>
      <#if field.dataType.baseType?lower_case == "bigdecimal" &&  !(visited?contains("minBigDecimal"))>
          <#assign visited += "minBigDecimal," />
import ${packageModel}.customvalidator.MinBigDecimal;
      <#elseif field.dataType.baseType?lower_case == "integer" &&  !(visited?contains("minInteger"))>
          <#assign visited += "minInteger," />
import ${packageModel}.customvalidator.MinInteger;
      <#elseif field.dataType.baseType?lower_case == "float" &&  !(visited?contains("minFloat"))>
          <#assign visited += "minFloat," />
import ${packageModel}.customvalidator.MinFloat;
      <#elseif field.dataType.baseType?lower_case == "double" &&  !(visited?contains("minDouble"))>
          <#assign visited += "minDouble," />
import ${packageModel}.customvalidator.MinDouble;
      </#if>
  </#if>
  <#if  field.restrictions.maxItems?has_content && !(visited?contains("maxItems"))>
      <#assign visited += "maxItems," />
import ${packageModel}.customvalidator.MaxItems;
  </#if>
  <#if  field.restrictions.minItems?has_content && !(visited?contains("minItems"))>
      <#assign visited += "minItems," />
import ${packageModel}.customvalidator.MinItems;
  </#if>
  <#if  field.restrictions.format?has_content && !(visited?contains("format"))>
      <#assign visited += "format,"  />
import com.fasterxml.jackson.annotation.JsonFormat;
  </#if>
  <#if (schema.schemaCombinator?has_content && (schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf")) && !(visited?contains("schemaCombinator"))>
      <#assign visited += "schemaCombinator," />
import ${exceptionPackage}.exception.ModelClassException;
  </#if>
  <#if  field.restrictions.pattern?has_content && !(visited?contains("pattern"))>
      <#assign visited +=  "pattern,"  />
import ${packageModel}.customvalidator.Pattern;
  </#if>
  <#if  field.restrictions.multipleOf?has_content && !(visited?contains("multipleOf"))>
      <#assign visited += "multipleOf,"  />
import ${packageModel}.customvalidator.MultipleOf;
  </#if>
  <#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true && !(visited?contains("uniqueItems"))>
      <#assign visited += "uniqueItems," />
import ${packageModel}.customvalidator.UniqueItems;
  </#if>
</#list>
import lombok.Builder;
<#list schema.fieldObjectList as field>
    <#if field.required>
import lombok.NonNull;
        <#break>
    </#if>
</#list>
<#list schema.fieldObjectList as field>
<#if field.dataType.baseType == "array" || field.dataType.baseType == "map">
import lombok.Singular;
    <#break>
</#if>
</#list>
import lombok.Value;
import lombok.extern.jackson.Jacksonized;
<#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import ${packageModel}.exception.ModelClassException;
</#if>

@Value
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${calculateSafeName (field.baseName, "")}")
<#if field.restrictions.minimum?has_content && (!field.restrictions.exclusiveMinimum?has_content)>
  ${calculateMinAnnotation (field.dataType)}(minimum = "${field.restrictions.minimum}", exclusive = false)
</#if>
<#if field.restrictions.minimum?has_content && field.restrictions.exclusiveMinimum?has_content>
  ${calculateMinAnnotation (field.dataType)}(minimum = "${field.restrictions.minimum}", exclusive = ${field.restrictions.exclusiveMinimum?string})
</#if>
<#if field.restrictions.maximum?has_content && (!field.restrictions.exclusiveMaximum?has_content)>
  ${calculateMaxAnnotation (field.dataType)}(maximum = "${field.restrictions.maximum}", exclusive = false)
</#if>
<#if field.restrictions.maximum?has_content && field.restrictions.exclusiveMaximum?has_content>
  ${calculateMaxAnnotation (field.dataType)}(maximum = "${field.restrictions.maximum}", exclusive = ${field.restrictions.exclusiveMaximum?string})
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
  @NonNull
</#if>
<#if field.dataType.baseType == "array">
<#assign singName>${singularizeName (calculateSafeName (field.baseName, ""))}</#assign>
  @Singular("${singName}")
  private ${field.dataType} ${field.baseName?uncap_first};
  <#elseif field.dataType.baseType == "map">
<#assign singName>${singularizeName (calculateSafeName (field.baseName, ""))}</#assign>
  @Singular("${singName}")
  private ${field.dataType} ${field.baseName?uncap_first};
<#elseif field.dataType.baseType == "enum">
  private ${field.baseName?cap_first} ${field.baseName?uncap_first};

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
  private ${field.dataType.innerType?cap_first} ${field.baseName?uncap_first};
  <#else>
  private ${field.dataType} ${createSimpleType (field)}
  </#if>

</#list>

  @Builder
  @Jacksonized
  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
      <#if field.required>@NonNull </#if><#if field.dataType.baseType == "enum">${field.baseName?cap_first} ${field.baseName?uncap_first}<#elseif field.dataType.baseType == "array">${field.dataType} ${field.baseName?uncap_first}<#elseif field.dataType.baseType == "map">${field.dataType} ${field.baseName?uncap_first}<#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">${field.dataType.innerType?cap_first} ${field.baseName?uncap_first}<#else>${field.dataType} ${field.baseName?uncap_first}</#if><#sep>, </#list>) {</@compress>
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
