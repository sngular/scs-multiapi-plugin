package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationWithoutOperationIds.assets;

import org.springframework.cloud.stream.function.StreamBridge;
import org.springframework.stereotype.Component;

@Component
public class StreamBridgeProducer {

    public StreamBridgeProducer(StreamBridge streamBridge){
      this.streamBridge = streamBridge;
    }

    private StreamBridge streamBridge;

    public void publishOperation(OrderCreatedDTO orderCreated){
        streamBridge.send("order.created", orderCreated);
    }

}