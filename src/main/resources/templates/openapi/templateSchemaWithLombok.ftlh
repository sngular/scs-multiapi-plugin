package ${packageModel};

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
import lombok.Builder;
import lombok.Data;
<#list schema.fieldObjectList as field>
  <#if field.required>
import lombok.NonNull;
    <#break>
  </#if>
</#list>

@Data
@Builder
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName}")
  <#if field.required?has_content && field.required == true>
  @NonNull
  </#if>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  private List<${field.dataType}> ${field.baseName} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  private Map<String,${field.dataType}> ${field.baseName} = new HashMap<String,${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "enum">
  private ${field.baseName?cap_first} ${field.baseName};

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
  private ${field.dataTypeSimple?cap_first} ${field.baseName};
  </#if>

</#list>
}
