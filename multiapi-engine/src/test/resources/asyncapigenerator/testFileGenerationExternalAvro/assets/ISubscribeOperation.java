package com.sngular.scsplugin.externalavro.model.event.consumer;

import com.sngular.scsplugin.externalavro.model.event.messages.CreateOrder;

public interface ISubscribeOperationExternalAvro {

  void subscribeOperationExternalAvro(final CreateOrder value);
}