package com.sngular.scsplugin.filegenerationwithkey.model.event.consumer;

import com.sngular.scsplugin.filegenerationwithkey.model.event.messages.OrderCreatedDTO;

public interface IPublishOperationFileGenerationWithKey {

  void publishOperationFileGenerationWithKey(final OrderCreatedDTO value);
}