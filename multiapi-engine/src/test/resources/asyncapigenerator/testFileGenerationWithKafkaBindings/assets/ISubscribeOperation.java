package com.sngular.scsplugin.fileGenerationWithKafkaBindings.model.event.producer;

import com.sngular.scsplugin.fileGenerationWithKafkaBindings.model.event.schemas.CreateOrderMapper;

public interface ISubscribeOperationFileGenerationWithKafkaBindings {

  CreateOrderMapper subscribeOperationFileGenerationWithKafkaBindings(String key);
}