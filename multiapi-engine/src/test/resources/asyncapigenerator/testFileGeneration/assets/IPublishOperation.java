package com.sngular.scsplugin.filegeneration.model.event.consumer;

import com.sngular.scsplugin.filegeneration.model.event.schemas.OrderDTO;

public interface IPublishOperationFileGeneration {

  void publishOperationFileGeneration(final OrderDTO value);
}