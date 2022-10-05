package net.coru.scsplugin.streambridge.model.event.consumer;

import net.coru.scsplugin.streambridge.model.event.CreateOrderDTO;

public interface ISubscribeOperationStreamBridge {

  void subscribeOperationStreamBridge(final CreateOrderDTO value);
}