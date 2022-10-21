package net.coru.scsplugin.filegeneration.model.event.consumer;

import net.coru.scsplugin.filegeneration.model.event.OrderCreatedDTO;

public interface IPublishOperationFileGeneration {

  void publishOperationFileGeneration(final OrderCreatedDTO value);
}