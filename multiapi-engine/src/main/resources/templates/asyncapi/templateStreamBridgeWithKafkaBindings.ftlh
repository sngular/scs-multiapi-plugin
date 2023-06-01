package ${streamBridgePackage};

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
<#list streamBridgeMethods as method>
<#if streamBridgeEntitiesSuffix?has_content>
import ${method.classNamespace}${streamBridgeEntitiesSuffix};
<#else>
import ${method.classNamespace};
</#if>
</#list>

@Component
public class ${streamBridgeClassName?cap_first} {

  private StreamBridge streamBridge;

  public ${streamBridgeClassName?cap_first}(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  <#list streamBridgeMethods as method>
  public void ${method.operationId?uncap_first}(final ${method.className}<#if streamBridgeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if> ${method.className?uncap_first}, final ${method.keyClassName}<#if streamBridgeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if> ${method.keyClassName?uncap_first}) {
    final var message = Message.build().payload(${method.className?uncap_first}).setHeader("key", ${method.keyClassName?uncap_first}).build();
    streamBridge.send("${method.channelName}", message);
  }

  </#list>
}