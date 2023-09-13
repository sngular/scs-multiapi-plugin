package smartylighting.streetlights.messaging.consumer;

import smartylighting.streetlights.messaging.consumer.model.TurnOnOffPayload;

public interface ITurnOff {

  void turnOff(final TurnOnOffPayload value);
}