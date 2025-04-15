package com.sngular.scsplugin.withoutids.model.event.consumer;

import com.sngular.scsplugin.withoutids.model.event.CreateOrderMessageDTO;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrderMessageDTO value);
}