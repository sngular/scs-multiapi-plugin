package ${subscribePackage};

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
<#list subscribeMethods as method>
import ${method.classNamespace}.${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>;
<#if method.keyClassNamespace?has_content>
import ${method.keyClassNamespace}.${method.keyClassName};
</#if>
</#list>
import ${subscribePackage}.MessageWrapper;
import org.springframework.messaging.Message;

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
  public Consumer<Message<${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>>> ${method.operationId?uncap_first}() {
    return value -> {
      final var messageWrapper = MessageWrapper.<${method.className}<#if subscribeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if>, ${method.keyClassName}>builder().payload(value.getPayload()).key(value.getKey()).build();
      ${method.operationId?uncap_first}.${method.operationId?uncap_first}(messageWrapper);
    }
  }

  </#list>

}
