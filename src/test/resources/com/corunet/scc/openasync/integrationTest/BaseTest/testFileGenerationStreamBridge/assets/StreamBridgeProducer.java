package com.corunet.scsplugin.business_model.model.event.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;

import com.corunet.scsplugin.business_model.model.event.CreateOrder;

@Component
public class StreamBridgeProducer {

    public StreamBridgeProducer(StreamBridge streamBridge){
      this.streamBridge = streamBridge;
    }

    private StreamBridge streamBridge;

    public void subscribeOperation(CreateOrder createOrder){
        streamBridge.send("order-createCommand", createOrder);
    }

}