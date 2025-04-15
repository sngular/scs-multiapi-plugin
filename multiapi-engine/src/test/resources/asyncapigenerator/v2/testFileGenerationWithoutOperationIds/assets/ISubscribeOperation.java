package com.sngular.scsplugin.withoutoperationids.model.event.consumer;

import com.sngular.scsplugin.withoutoperationids.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

  void subscribeOperation(final CreateOrderDTO value);
}