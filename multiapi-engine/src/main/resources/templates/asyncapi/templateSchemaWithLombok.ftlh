<#ftl output_format="plainText">
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
        ${calculateSafeName (field.baseName, ";", field.const)}
    <#else >
        ${calculateSafeName (field.baseName, ";", "")}
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
        @Size(min =0, max =${field.restrictionProperties.maxLength})
    <#elseif !field.restrictionProperties.maxLength?has_content && field.restrictionProperties.minLength?has_content>
        @Size(min =${field.restrictionProperties.minLength}, max =0)
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
    <#if field.dataType.baseType == "array">
        private ${field.dataType} ${field.baseName?uncap_first} = new ${field.dataType?api.getImplementationTypeString()}();
    <#elseif field.dataType.baseType == "map">
        private ${field.dataType} ${field.baseName?uncap_first} = new ${field.dataType?api.getImplementationTypeString()}();
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
