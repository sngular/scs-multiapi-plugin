package com.sngular.scsplugin.reservedwordsgeneration.model.event.consumer;

import com.sngular.scsplugin.reservedwordsgeneration.model.event.CreateOrderDTO;

public interface ISubscribeOperationFileGeneration {

  void subscribeOperationFileGeneration(final CreateOrderDTO value);
}