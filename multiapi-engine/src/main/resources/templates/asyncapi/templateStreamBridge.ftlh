package ${streamBridgePackage};

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
<#list streamBridgeMethods as method>
import ${method.classNamespace}.${method.className}<#if streamBridgeEntitiesSuffix?has_content>${streamBridgeEntitiesSuffix}</#if>;
</#list>

@Component
public class ${streamBridgeClassName?cap_first} {

  private StreamBridge streamBridge;

  public ${streamBridgeClassName?cap_first}(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  <#list streamBridgeMethods as method>
  public void ${method.operationId?uncap_first}(final ${method.className}<#if streamBridgeEntitiesSuffix?has_content>${subscribeEntitiesSuffix}</#if> ${method.className?uncap_first}) {
    streamBridge.send("${method.channelName}", ${method.className?uncap_first});
  }

  </#list>
}