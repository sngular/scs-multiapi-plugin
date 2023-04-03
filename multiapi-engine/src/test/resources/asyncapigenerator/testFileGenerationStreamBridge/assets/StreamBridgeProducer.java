package com.sngular.scsplugin.streambridge.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
import com.sngular.scsplugin.streambridge.model.event.messages.OrderCreated;

@Component
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public StreamBridgeProducer(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  public void publishOperationStreamBridge(final OrderCreated orderCreated) {
    streamBridge.send("order.created", orderCreated);
  }

}