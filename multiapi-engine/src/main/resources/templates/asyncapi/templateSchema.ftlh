package ${packageModel};

import java.util.Objects;

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
    <#if field.required || schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
import ${packageModel}.exception.ModelClassException;
    <#break>
    </#if>
</#list>

public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName?uncap_first}")
  <#if field.dataType?has_content && (field.dataTypeSimple == "array" || field.dataTypeSimple == "Array")>
    <#if field.required>
  private final List<${field.dataType}> ${field.baseName?uncap_first};
    <#else>
  private List<${field.dataType}> ${field.baseName?uncap_first} = new ArrayList<${field.dataType}>();
    </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    <#if field.required>
  private final Map<String, ${field.dataType}> ${field.baseName?uncap_first};
    <#else>
  private Map<String, ${field.dataType}> ${field.baseName?uncap_first} = new HashMap<String, ${field.dataType}>();
    </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    <#if field.required>
  private final ${field.baseName?cap_first} ${field.baseName?uncap_first};
    <#else>
  private ${field.baseName?cap_first} ${field.baseName?uncap_first};
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
  private final ${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first};
    <#else>
  private ${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first};
    </#if>
  </#if>
</#list>

  private ${schema.className}(<@compress single_line=true><#list schema.fieldObjectList as field>
    <#if field.dataType?has_content && field.dataTypeSimple == "enum">${field.baseName?cap_first} ${field.baseName?uncap_first}<#elseif field.dataType?has_content && field.dataTypeSimple == "array">List<${field.dataType}> ${field.baseName?uncap_first}<#elseif field.dataType?has_content && field.dataTypeSimple == "map">Map<String, ${field.dataType}> ${field.baseName?uncap_first}<#else>${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first}</#if><#sep>, </#list>) {</@compress>
    <#list schema.fieldObjectList as field>
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
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
    this.${field.baseName?uncap_first} = builder.${field.baseName?uncap_first};
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

  public static class ${schema.className}Builder {

<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
    private List<${field.dataType}> ${field.baseName?uncap_first} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    private Map<String, ${field.dataType}> ${field.baseName?uncap_first} = new HashMap<String,${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    private ${field.baseName?cap_first} ${field.baseName?uncap_first};
  <#else>
    private ${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first};
  </#if>
</#list>
<#list schema.fieldObjectList as field>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
    public ${schema.className}.${schema.className}Builder ${field.baseName}(List<${field.dataType}> ${field.baseName}) {
      if (!${field.baseName?uncap_first}.isEmpty()) {
        this.${field.baseName?uncap_first}.addAll(${field.baseName});
      }
      return this;
    }

    public ${schema.className}.${schema.className}Builder ${field.baseName?substring(0, field.baseName?length - 1)}(${field.dataType} ${field.baseName?substring(0, field.baseName?length - 1)}) {
      if (${field.baseName?substring(0, field.baseName?length - 1)} != null) {
        this.${field.baseName?uncap_first}.add(${field.baseName?substring(0, field.baseName?length - 1)});
      }
      return this;
    }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
    public ${schema.className}.${schema.className}Builder ${field.baseName}(Map<String, ${field.dataType}> ${field.baseName?uncap_first}) {
      this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
      return this;
    }

    public ${schema.className}.${schema.className}Builder ${field.baseName}Value(String key, ${field.dataType} value) {
      this.${field.baseName?uncap_first}.put(key, value);
      return this;
    }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
    public ${schema.className}.${schema.className}Builder ${field.baseName}(${field.baseName?cap_first} ${field.baseName}) {
      this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
      return this;
    }
  <#else>

    public ${schema.className}.${schema.className}Builder ${field.baseName?uncap_first}(${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first}) {
      this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
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
  public List<${field.dataType}> get${field.baseName?cap_first}() {
    return ${field.baseName?uncap_first};
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(List<${field.dataType}> ${field.baseName?uncap_first}) {
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
  }
  </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  public Map<String, ${field.dataType}> get${field.baseName?cap_first}() {
    return ${field.baseName?uncap_first};
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(Map<String, ${field.dataType}> ${field.baseName?uncap_first}) {
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
  }
  </#if>
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
  public ${field.baseName?cap_first} get${field.baseName?cap_first}() {
    return ${field.baseName?uncap_first};
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(${field.baseName?cap_first} ${field.baseName?uncap_first}) {
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
  }
  </#if>
  <#elseif field.dataTypeSimple?has_content>
  public ${field.dataTypeSimple?cap_first} get${field.baseName?cap_first}() {
    return ${field.baseName?uncap_first};
  }
  <#if field.required == false>
  public void set${field.baseName?cap_first}(${field.dataTypeSimple?cap_first} ${field.baseName?uncap_first}) {
    this.${field.baseName?uncap_first} = ${field.baseName?uncap_first};
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
    return <#compress><#list schema.fieldObjectList as field> Objects.equals(this.${field.baseName?uncap_first}, ${schema.className?uncap_first}.${field.baseName?uncap_first})<#if field?has_next> && </#if></#list>;</#compress>
  }

  @Override
  public int hashCode() {
    return Objects.hash(<#list schema.fieldObjectList as field>${field.baseName?uncap_first}<#if field?has_next>, </#if></#list>);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ${schema.className} {\n");
    <#list schema.fieldObjectList as field>
    sb.append(" ${field.baseName?uncap_first}: ").append(toIndentedString(${field.baseName?uncap_first})).append("\n");
    </#list>
    sb.append("}");
    return sb.toString();
  }

  /**
  * Convert the given object to string with each line indented by 4 spaces
  * (except the first line).
  */
  private String toIndentedString(Object o) {
    if (o == null) {
      return "null";
    }
    return o.toString().replace("\n", "\n ");
  }

  <#if schema.schemaCombinator == "anyOf" || schema.schemaCombinator == "oneOf">
  private void validatePartialCombinations() {
    boolean satisfiedCondition = false;

  <#list schema.fieldObjectList as field>
    <#if field == schema.fieldObjectList?first>
    if (Objects.nonNull(this.${field.baseName?uncap_first})) {
    <#else> else if (Objects.nonNull(this.${field.baseName?uncap_first})) {
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
    if (!Objects.nonNull(this.${field.baseName?uncap_first})) {
    <#else>
    else if (!Objects.nonNull(this.${field.baseName?uncap_first})) {
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
