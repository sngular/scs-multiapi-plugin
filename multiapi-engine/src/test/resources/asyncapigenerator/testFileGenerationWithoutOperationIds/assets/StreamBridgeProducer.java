package com.sngular.scsplugin.withoutids.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
import com.sngular.scsplugin.withoutids.model.event.schemas.OrderMapper;

@Component
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public StreamBridgeProducer(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

  public void publishOperation(final OrderDTO order) {
    streamBridge.send("order.created", order);
  }

}