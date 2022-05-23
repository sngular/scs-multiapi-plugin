package ${streamBridgePackage};

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;

<#list streamBridgeMethods as method>
<#if streamBridgeEntitiesPostfix?has_content>
import ${method.classNamespace}${streamBridgeEntitiesPostfix};
<#else>
import ${method.classNamespace};
</#if>
</#list>

@Component
public class ${streamBridgeClassName?cap_first} {

    public ${streamBridgeClassName?cap_first}(StreamBridge streamBridge){
      this.streamBridge = streamBridge;
    }

    private StreamBridge streamBridge;

    <#list streamBridgeMethods as method>
    public void ${method.operationId?uncap_first}(${method.className}<#if streamBridgeEntitiesPostfix?has_content>${subscribeEntitiesPostfix}</#if> ${method.className?uncap_first}){
        streamBridge.send("${method.channelName}", ${method.className?uncap_first});
    }

    </#list>
}