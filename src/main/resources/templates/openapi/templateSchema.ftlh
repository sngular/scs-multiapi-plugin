package ${packageModel};

import java.util.Objects;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.annotations.ApiModelProperty;
<#list schema.importList as import>
import ${import};
</#list>

public class ${schema.className} {

<#list schema.fieldObjectList as field>
  @JsonProperty(value ="${field.baseName}")
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  private List<${field.dataType}> ${field.baseName} = new ArrayList<${field.dataType}>();
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  private HashMap<String,${field.dataType}> ${field.baseName} = new HashMap<String,${field.dataType}>();
  <#else>
  private ${field.dataTypeSimple?cap_first} ${field.baseName};
   </#if>
</#list>

<#list schema.fieldObjectList as field>
  /**
  * Get ${field.baseName?uncap_first}
  * @return ${field.baseName?uncap_first}
  */
  @ApiModelProperty(<#if field.required?has_content> = ${field.required}, </#if> value = "description")
  <#if field.required?has_content && field.required == true>
  @NotNull
  </#if>
  <#if field.dataType?has_content && field.dataTypeSimple == "array">
  public List<${field.dataType}> get${field.baseName?cap_first}() {
    return ${field.baseName};
  }
  public void set${field.baseName?cap_first}(ArrayList<${field.dataType}> ${field.baseName}) {
    this.${field.baseName} = ${field.baseName};
  }
  <#elseif field.dataType?has_content && field.dataTypeSimple == "map">
  public HashMap<String,${field.dataType}> get${field.baseName?cap_first}() {
    return ${field.baseName};
  }
  public void set${field.baseName?cap_first}(HashMap<String,${field.dataType}> ${field.baseName}) {
    this.${field.baseName} = ${field.baseName};
  }
  <#else>
  public ${field.dataTypeSimple?cap_first} get${field.baseName?cap_first}() {
    return ${field.baseName};
  }
  public void set${field.baseName?cap_first}(${field.dataTypeSimple?cap_first} ${field.baseName}) {
    this.${field.baseName} = ${field.baseName};
  }
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
    return <#compress><#list schema.fieldObjectList as field> Objects.equals(this.${field.baseName?uncap_first},${schema.className?uncap_first}.${field.baseName?uncap_first}) <#if field?has_next> && </#if></#list>;</#compress>
  }

  @Override
  public int hashCode() {
    return Objects.hash(<#list schema.fieldObjectList as field>${field.baseName?uncap_first}<#if field?has_next>,</#if></#list>);
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("class ${schema.className} ${stringBracketOpen}\n");
    <#list schema.fieldObjectList as field>
    sb.append(" ${field.baseName?uncap_first}: ").append(toIndentedString(${field.baseName?uncap_first})).append("\n");
    </#list>
    sb.append("${stringBracketClose}");
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

}
