package com.sngular.scsplugin.modelclass.model.event.consumer;

import com.sngular.scsplugin.modelclass.model.event.messages.CreateOrderEventDTO;

public interface ISubscribeOrder {

  void subscribeOrder(final CreateOrderEventDTO value);
}