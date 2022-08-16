package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationStreamBridge.assets;

import net.coru.scsplugin.business_model.model.event.OrderCreated;
import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class StreamBridgeProducer {

    public StreamBridgeProducer(StreamBridge streamBridge){
      this.streamBridge = streamBridge;
    }

    private StreamBridge streamBridge;

    public void publishOperation(OrderCreated orderCreated){
        streamBridge.send("order.created", orderCreated);
    }

}