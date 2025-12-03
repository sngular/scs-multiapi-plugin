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
<#-- Return a safe variable name converted to lowerCamel then checked for reserved words/digits -->
<#function varSafeName propertyName ender const="">
  <#return calculateSafeName(templateUtils.toLowerCamel(propertyName), ender, const)/>
</#function>
<#function createSimpleType field>
    ${varSafeName(field.baseName, ";", field.constValue)}
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

import com.fasterxml.jackson.annotation.JsonValue;

public enum ${schema.className} {
<#list schema.fieldObjectList as field>
<#list field.enumValues as key, value>
<#if key?matches("[0-9].*")>
  ${"_"+key}(${value})<#sep>,
<#else>
  ${key}(${value})<#sep>,
</#if>
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
</#list>
}