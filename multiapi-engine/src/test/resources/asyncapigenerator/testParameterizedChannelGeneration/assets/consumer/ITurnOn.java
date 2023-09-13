package smartylighting.streetlights.messaging.consumer;

import smartylighting.streetlights.messaging.consumer.model.TurnOnOffPayload;

public interface ITurnOn {

  void turnOn(final TurnOnOffPayload value);
}