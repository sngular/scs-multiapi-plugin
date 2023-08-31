package com.sngular.scsplugin.streambridge.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
import com.sngular.scsplugin.streambridge.model.event.Order;

@Component
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public StreamBridgeProducer(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  public void publishOperationStreamBridge(final Order order) {
    streamBridge.send("order.created", order);
  }

}