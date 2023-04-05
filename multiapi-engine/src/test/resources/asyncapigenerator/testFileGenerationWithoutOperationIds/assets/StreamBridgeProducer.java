package com.sngular.scsplugin.withoutids.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
import com.sngular.scsplugin.withoutids.model.event.messages.OrderCreatedMapper;

@Component
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public StreamBridgeProducer(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  public void publishOperation(final OrderCreatedDTO orderCreated) {
    streamBridge.send("order.created", orderCreated);
  }

}