package com.sngular.scsplugin.rarecharsgeneration.model.event.consumer;

import com.sngular.scsplugin.rarecharsgeneration.model.event.CreateOrderDTO;

public interface ISubscribeOperationFileGeneration {

  void subscribeOperationFileGeneration(final CreateOrderDTO value);
}