<#ftl output_format="plainText">
<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]/>
<#assign collection = ["array","map"]/>
<#function calculateSafeName propertyName ender const="">
    <#if words?seq_contains(propertyName?uncap_first) || propertyName?matches("[0-9].*")>
        <#if const?has_content>
            <#return getConstValue ("_"+propertyName?uncap_first, ender, const) />
        <#else>
            <#return getConstValue ("_"+propertyName?uncap_first, ender) />
        </#if>
    <#else >
        <#if const?has_content>
            <#return getConstValue( propertyName?uncap_first, ender, const) />
        <#else>
            <#return getConstValue( propertyName?uncap_first, ender) />
        </#if>
    </#if>
</#function>
<#function createSimpleType field>
    <#if (field.constValue?has_content)>
        ${calculateSafeName (field.fieldName, ";", field.constValue)}
    <#else >
        ${calculateSafeName (field.fieldName, ";", "")}
    </#if>
</#function>
<#function getConstValue fieldName ender value="">
    <#if value?has_content>
        <#return fieldName +" = " + renderValue (value) + ender/>
    <#else >
        <#return fieldName + ender/>
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
<#function calculateSingularSafeName propertyName>
    <#local pluralName = calculateSafeName (propertyName, "")>
    <#local singularName = calculateSafeName (singularizeName (propertyName), "")>
    <#if singularName == pluralName>
        <#return "_" + singularName/>
    <#else>
        <#return singularName/>
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
<#if usePactAnnotation>
import com.sngular.annotation.pact.PactDslBodyBuilder;
</#if>
<#list schema.fieldObjectList as field>
    <#if field.dataType.baseType == "enum" && field.unknownEnumFallback>
import com.fasterxml.jackson.annotation.JsonCreator;
        <#break>
    </#if>
</#list>
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
  <#if collection?seq_contains(field.dataType.baseType)>
import lombok.Singular;
  <#break>
</#if>
</#list>
import lombok.Value;
import lombok.extern.jackson.Jacksonized;

<#if usePactAnnotation>
@PactDslBodyBuilder
</#if>
@Value
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName?uncap_first}")
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
  @MaxItems(maximum = ${field.restrictions.maxItems?c})
</#if>
<#if field.restrictions.minItems?has_content>
  @MinItems(minimum = ${field.restrictions.minItems?c})
</#if>
<#if field.restrictions.format?has_content>
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "${field.restrictions.format}")
</#if>
<#if field.restrictions.uniqueItems?has_content && field.restrictions.uniqueItems == true>
  @UniqueItems
</#if>
<#if field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(min =${field.restrictions.minLength?c}, max =${field.restrictions.maxLength?c})
<#elseif field.restrictions.maxLength?has_content && !field.restrictions.minLength?has_content>
  @Size(max =${field.restrictions.maxLength?c}, min =0)
<#elseif !field.restrictions.maxLength?has_content && field.restrictions.minLength?has_content>
  @Size(max =Integer.MAX_VALUE, min =${field.restrictions.minLength?c})
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
<#assign singName>${calculateSingularSafeName (field.fieldName)}</#assign>
  @Singular(value = "${singName}", ignoreNullCollections = true)
  private ${field.dataType} ${field.fieldName?uncap_first};
  <#elseif field.dataType.baseType == "map">
<#assign singName>${calculateSingularSafeName (field.fieldName)}</#assign>
  @Singular("${singName}")
  private ${field.dataType} ${field.fieldName?uncap_first};
<#elseif field.dataType.baseType == "enum">
  private ${field.fieldName?cap_first} ${field.fieldName?uncap_first};

  public enum ${field.fieldName?cap_first} {
<#assign enumName = field.fieldName?cap_first>
<#assign valueType = field.dataType.innerType?cap_first>
<#assign stringEnum = valueType == "String">
<#assign addUnknown = field.unknownEnumFallback && !field.enumValues?keys?seq_contains("UNKNOWN")>
<#list field.enumValues as key, value>
  <#if key?matches("[0-9].*")>
    ${"_"+key}(${value})<#sep>,
  <#else>
    ${key}(${value})<#sep>,
  </#if>
</#list><#if addUnknown>,
    /** A value the contract does not declare, written back as {@code "UNKNOWN"}<#if !stringEnum>; {@link #getValue()} returns {@code null}</#if>. */
    UNKNOWN(<#if stringEnum>"UNKNOWN"<#else>null</#if>)</#if>;

    private ${valueType} value;

    ${enumName}(${valueType} value) {
      this.value = value;
    }

<#if field.unknownEnumFallback && !stringEnum>
    /** The contract value; {@code null} for {@link #UNKNOWN}. */
    public ${valueType} getValue() {
      return value;
    }

    @JsonValue
    public Object toJsonValue() {
      return this == UNKNOWN ? "UNKNOWN" : value;
    }

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static ${enumName} fromValue(Object value) {
      if (value == null) {
        return null;
      }
      final java.math.BigDecimal number;
      try {
        number = new java.math.BigDecimal(value.toString());
      } catch (NumberFormatException e) {
        return UNKNOWN;
      }
      for (${enumName} constant : values()) {
        if (constant.value != null && number.compareTo(new java.math.BigDecimal(constant.value.toString())) == 0) {
          return constant;
        }
      }
      return UNKNOWN;
    }

    @Override
    public String toString() {
      return this == UNKNOWN ? "UNKNOWN" : String.valueOf(value);
    }
<#else>
    @JsonValue
    public ${valueType} getValue() {
      return value;
    }
<#if field.unknownEnumFallback>

    /** Whether this is the constant that values outside the contract resolve to. */
    public boolean isUnknown() {
      return this == UNKNOWN;
    }

    @JsonCreator(mode = JsonCreator.Mode.DELEGATING)
    public static ${enumName} fromValue(String value) {
      if (value == null) {
        return null;
      }
      for (${enumName} constant : values()) {
        if (value.equals(constant.value)) {
          return constant;
        }
      }
      return UNKNOWN;
    }
</#if>

    @Override
    public String toString() {
      return String.valueOf(value);
    }
</#if>
  }
<#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">
  private ${field.dataType.innerType?cap_first} ${field.fieldName?uncap_first};
<#else>
  private ${field.dataType} ${createSimpleType (field)}
</#if>

</#list>

  @Builder
  @Jacksonized
  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
      <#if field.required>@NonNull </#if><#if field.dataType.baseType == "enum">${field.fieldName?cap_first} ${field.fieldName?uncap_first}<#elseif field.dataType.baseType == "array">${field.dataType} ${field.fieldName?uncap_first}<#elseif field.dataType.baseType == "map">${field.dataType} ${field.fieldName?uncap_first}<#elseif field.dataType.innerType?has_content && field.dataType.baseType == "object">${field.dataType.innerType?cap_first} ${field.fieldName?uncap_first}<#else>${field.dataType} ${field.fieldName?uncap_first}</#if><#sep>, </#list>) {</@compress>
  <#list schema.fieldObjectList as field>
    this.${field.fieldName?uncap_first} = ${field.fieldName?uncap_first};
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
            if (Objects.nonNull(this.${field.fieldName?uncap_first})) {<#else> else if (Objects.nonNull(this.${field.fieldName?uncap_first})) {</#if>
        satisfiedCondition = true;
        }</#list>

    if (!satisfiedCondition) {
    throw new ModelClassException("${schema.className}");
    }
  }
</#if>
}
