package net.coru.scsplugin.withOutIds.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;

import net.coru.scsplugin.withOutIds.model.event.OrderCreatedMapper;

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