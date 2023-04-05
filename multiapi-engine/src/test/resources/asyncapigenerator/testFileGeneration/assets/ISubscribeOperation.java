package com.sngular.scsplugin.filegeneration.model.event.producer;

import com.sngular.scsplugin.filegeneration.model.event.schemas.CreateOrderMapper;

public interface ISubscribeOperationFileGeneration {

  CreateOrderMapper subscribeOperationFileGeneration();
}