package com.sngular.scsplugin.filegeneration.model.event.consumer;

import com.sngular.scsplugin.filegeneration.model.event.OrderDTO;

public interface IPublishOperationFileGeneration {

  void publishOperationFileGeneration(final OrderDTO value);
}