package com.sngular.scsplugin.streambridge.model.event.consumer;

import com.sngular.scsplugin.streambridge.model.event.CreateOrderMessageDTO;

public interface ISubscribeOperationStreamBridge {

  void subscribeOperationStreamBridge(final CreateOrderMessageDTO value);
}