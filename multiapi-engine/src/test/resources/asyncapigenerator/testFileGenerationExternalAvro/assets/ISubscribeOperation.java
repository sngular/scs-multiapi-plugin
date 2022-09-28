package net.coru.scsplugin.externalavro.model.event.consumer;

import net.coru.scsplugin.externalavro.model.event.CreateOrder;

public interface ISubscribeOperationExternalAvro {

  void subscribeOperationExternalAvro(final CreateOrder value);
}