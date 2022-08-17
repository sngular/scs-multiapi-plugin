package net.coru.scsplugin.business_model.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;

import net.coru.scsplugin.business_model.model.event.OrderCreated;

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