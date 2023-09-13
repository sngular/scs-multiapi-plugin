package smartylighting.streetlights.messaging.consumer;

import smartylighting.streetlights.messaging.consumer.model.DimLightPayload;

public interface IDimLight {

  void dimLight(final DimLightPayload value);
}