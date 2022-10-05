package net.coru.scsplugin.withoutids.model.event.consumer;

import net.coru.scsplugin.withoutids.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrderDTO value);
}