package ${supplierPackage};

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
<#list publishMethods as method>
import ${method.classNamespace}.${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>;
<#if method.keyClassNamespace?has_content>
import ${method.keyClassNamespace}.${method.keyClassName};
</#if></#list>
import ${wrapperPackage}.MessageWrapper;

@Configuration
public class ${supplierClassName?cap_first} {

  <#list publishMethods as method>
  private final I${method.operationId?cap_first} ${method.operationId?uncap_first};

  private final ${method.keyClassName} key;

  </#list>
  protected ${supplierClassName?cap_first}(<#list publishMethods as method>final I${method.operationId?cap_first} ${method.operationId?uncap_first}<#sep>, </#list>) {
    <#list publishMethods as method>
    this.${method.operationId?uncap_first} = ${method.operationId?uncap_first};
    </#list>
  }

  <#list publishMethods as method>
  @Bean
  public Supplier<Message<${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>>> ${method.operationId?uncap_first}() {
    final var messageWrapper =  ${method.operationId?uncap_first}.${method.operationId?uncap_first}();
    return () -> Message.build().payload((${method.className}<#if supplierEntitiesSuffix?has_content>${supplierEntitiesSuffix}</#if>) messageWrapper.getPayload()).setHeader("key", (${method.keyClassName}) messageWrapper.getKey()).build();
  }

  </#list>

}
