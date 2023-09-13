package smartylighting.streetlights.messaging.producer;

import org.springframework.stereotype.Component;
import org.springframework.cloud.stream.function.StreamBridge;
import smartylighting.streetlights.messaging.producer.model.LightMeasuredPayloadDTO;

@Component
public class StreamBridgeProducer {

  private StreamBridge streamBridge;

  public StreamBridgeProducer(final StreamBridge streamBridge) {
    this.streamBridge = streamBridge;
  }

      public void receiveLightMeasurement(final LightMeasuredPayloadDTO lightMeasuredPayload, final String... parameters) {
      streamBridge.send(String.format("smartylighting.streetlights.1.0.event.%s.lighting.measured", parameters), lightMeasuredPayload);
      }

}