package ${supplierPackage};

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
<#list publishMethods as method>
<#if supplierEntitiesSuffix?has_content>
import ${method.classNamespace}${supplierEntitiesSuffix};
<#else>
import ${method.classNamespace};
</#if>
</#list>

@Configuration
public class ${supplierClassName?cap_first} {

  <#list publishMethods as method>
  private final I${method.operationId?cap_first} ${method.operationId?uncap_first};

  </#list>
  protected ${supplierClassName?cap_first}(<#list publishMethods as method>final I${method.operationId?cap_first} ${method.operationId?uncap_first}<#sep>, </#list>) {
    <#list publishMethods as method>
    this.${method.operationId?uncap_first} = ${method.operationId?uncap_first};
    </#list>
  }

  <#list publishMethods as method>
  @Bean
  public Supplier<${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>> ${method.operationId?uncap_first}() {
    return () -> ${method.operationId?uncap_first}.${method.operationId?uncap_first}();
  }

  </#list>

}
