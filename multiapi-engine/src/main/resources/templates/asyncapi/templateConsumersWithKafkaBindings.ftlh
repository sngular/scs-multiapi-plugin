package ${subscribePackage};

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
<#list subscribeMethods as method>
<#if subscribeEntitiesSuffix?has_content>
import ${method.classNamespace}${subscribeEntitiesSuffix};
<#else>
import ${method.classNamespace};
</#if>
</#list>
import ${subscribePackage}.MessageWrapper<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>;

@Configuration
public class ${subscribeClassName?cap_first} {

  <#list subscribeMethods as method>
  private final I${method.operationId?cap_first} ${method.operationId?uncap_first};

  </#list>

  protected ${subscribeClassName?cap_first}(<#list subscribeMethods as method>final I${method.operationId?cap_first} ${method.operationId?uncap_first}<#sep>, </#list>) {
    <#list subscribeMethods as method>
    this.${method.operationId?uncap_first} = ${method.operationId?uncap_first};
    </#list>
  }

  <#list subscribeMethods as method>
  @Bean
  public Consumer<Message<${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>> ${method.operationId?uncap_first}() {
    return value -> {
      final var messageWrapper = MessageWrapper.builder().payload(value.getPayload()).key(value.getKey()).build();
      ${method.operationId?uncap_first}.${method.operationId?uncap_first}(messageWrapper);
    }
  }

  </#list>

}
