package com.sngular.scsplugin.streambridge.model.event.consumer;

import com.sngular.scsplugin.streambridge.model.event.messages.CreateOrderDTO;

public interface ISubscribeOperationStreamBridge {

  void subscribeOperationStreamBridge(final CreateOrderDTO value);
}