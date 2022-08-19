package net.coru.scsplugin.streambridge.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;

import net.coru.scsplugin.streambridge.model.event.OrderCreated;

@Component
public class StreamBridgeProducer {

    public StreamBridgeProducer(StreamBridge streamBridge){
      this.streamBridge = streamBridge;
    }

    private StreamBridge streamBridge;

    public void publishOperationStreamBridge(OrderCreated orderCreated){
        streamBridge.send("order.created", orderCreated);
    }

}