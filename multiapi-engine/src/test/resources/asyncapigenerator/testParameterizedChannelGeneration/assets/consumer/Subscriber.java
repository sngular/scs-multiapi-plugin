package smartylighting.streetlights.messaging.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import smartylighting.streetlights.messaging.consumer.model.TurnOnOffPayload;
import smartylighting.streetlights.messaging.consumer.model.TurnOnOffPayload;
import smartylighting.streetlights.messaging.consumer.model.DimLightPayload;

@Configuration
public class Subscriber {

  private final ITurnOn turnOn;

  private final ITurnOff turnOff;

  private final IDimLight dimLight;

  protected Subscriber(final ITurnOn turnOn, final ITurnOff turnOff, final IDimLight dimLight) {
    this.turnOn = turnOn;
    this.turnOff = turnOff;
    this.dimLight = dimLight;
  }

  @Bean
  public Consumer<TurnOnOffPayload> turnOn() {
    return value -> turnOn.turnOn(value);
  }

  @Bean
  public Consumer<TurnOnOffPayload> turnOff() {
    return value -> turnOff.turnOff(value);
  }

  @Bean
  public Consumer<DimLightPayload> dimLight() {
    return value -> dimLight.dimLight(value);
  }


}
