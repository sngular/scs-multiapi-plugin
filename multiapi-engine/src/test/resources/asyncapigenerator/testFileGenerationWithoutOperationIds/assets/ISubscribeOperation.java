package com.sngular.scsplugin.withoutids.model.event.consumer;

import com.sngular.scsplugin.withoutids.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrderDTO value);
}