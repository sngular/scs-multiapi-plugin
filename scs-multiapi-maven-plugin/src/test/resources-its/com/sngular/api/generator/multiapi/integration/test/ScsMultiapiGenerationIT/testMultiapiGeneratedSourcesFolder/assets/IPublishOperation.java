package com.sngular.generator.multiapi.model.event.consumer;

import com.sngular.generator.multiapi.model.event.OrderDTO;

public interface IPublishOperation {

  void publishOperation(final OrderDTO value);
}