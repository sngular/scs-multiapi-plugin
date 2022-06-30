package ${packageModel};

import com.fasterxml.jackson.annotation.JsonProperty;
<#list schema.importList as import>
import ${import};
</#list>
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName}")
  <#if field.required?has_content && field.required == true>
  @NotNull
  </#if>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  private List<${field.dataType}> ${field.baseName} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  private Map<String,${field.dataType}> ${field.baseName} = new HashMap<String,${field.dataType}>();
  <#elseif field.dataTypeSimple?has_content>
  private ${field.dataTypeSimple?cap_first} ${field.baseName};
  </#if>
</#list>

}
