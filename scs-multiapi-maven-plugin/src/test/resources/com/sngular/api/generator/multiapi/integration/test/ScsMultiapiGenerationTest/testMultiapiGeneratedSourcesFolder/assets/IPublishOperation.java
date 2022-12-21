package com.sngular.generator.multiapi.model.event.consumer;

import com.sngular.generator.multiapi.model.event.OrderCreatedDTO;

public interface IPublishOperation {

  void publishOperation(final OrderCreatedDTO value);
}