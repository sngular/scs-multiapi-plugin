package ${subscribePackage};

import java.util.function.Consumer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

<#list subscribeMethods as method>
<#if subscribeEntitiesPostfix?has_content>
import ${method.classNamespace}${subscribeEntitiesPostfix};
<#else>
import ${method.classNamespace};
</#if>
</#list>

@Configuration
public class ${subscribeClassName?cap_first} {

    <#list subscribeMethods as method>
    private final I${method.operationId?cap_first} ${method.operationId?uncap_first};

    </#list>
    protected ${subscribeClassName?cap_first}(<#list subscribeMethods as method>final I${method.operationId?cap_first} ${method.operationId?uncap_first}<#sep>, </#list>){
      <#list subscribeMethods as method>
      this.${method.operationId?uncap_first} = ${method.operationId?uncap_first};
      </#list>
    }

    <#list subscribeMethods as method>
    @Bean
    public Consumer<${method.className}<#if subscribeEntitiesPostfix?has_content>${subscribeEntitiesPostfix}</#if>> ${method.operationId?uncap_first}(){ return value -> ${method.operationId?uncap_first}.${method.operationId?uncap_first}(value); }

    </#list>

}
