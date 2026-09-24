<#ftl output_format="plainText">
<#assign words = ["abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "double", "do", "else", "enum", "extends", "false", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "void", "volatile", "while"]>
<#function calculateSafeName propertyName ender const="">
    <#if words?seq_contains(propertyName?uncap_first) || propertyName?matches("[0-9].*")>
        <#if const?has_content>
            <#return getConstValue("_"+propertyName, ender, const)/>
        <#else>
            <#return getConstValue ("_"+propertyName, ender)/>
        </#if>
    <#else >
        <#return getConstValue(propertyName, ender, const)/>
    </#if>
</#function>
<#function createSimpleType field>
    ${calculateSafeName (field.baseName, ";", field.constValue)}
</#function>
<#function singularizeName propertyName>
    <#if propertyName?matches(".*ies")>
        <#return propertyName?remove_ending("ies")+"y"/>
    <#elseif propertyName?matches(".*s")>
        <#return propertyName?remove_ending("s")/>
    <#elseif propertyName?matches(".*List")>
        <#return propertyName?remove_ending("List")/>
    <#else >
        <#return  "_"+propertyName/>
    </#if>
</#function>
<#function hasConstValue field>
    <#return field.constValue?has_content />
</#function>
<#function requireFinal field>
    <#if field.required == true || hasConstValue(field)>
        <#return "final "/>
    <#else >
        <#return ""/>
    </#if>
</#function>
<#function checkIfShouldSet field>
  <<#return !(field.required == true || hasConstValue(field))>
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
package ${packageModel};

<#list schema.fieldObjectList as field>
  <#if field.unknownEnumFallback>
import com.fasterxml.jackson.annotation.JsonCreator;
    <#break>
  </#if>
</#list>
import com.fasterxml.jackson.annotation.JsonValue;

public enum ${schema.className} {
<#list schema.fieldObjectList as field>
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

  ${schema.className?cap_first}(${valueType} value) {
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
  public static ${schema.className} fromValue(Object value) {
    if (value == null) {
      return null;
    }
    final java.math.BigDecimal number;
    try {
      number = new java.math.BigDecimal(value.toString());
    } catch (NumberFormatException e) {
      return UNKNOWN;
    }
    for (${schema.className} constant : values()) {
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
  public static ${schema.className} fromValue(String value) {
    if (value == null) {
      return null;
    }
    for (${schema.className} constant : values()) {
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
</#list>
}