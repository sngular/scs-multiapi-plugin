package com.sngular.scsplugin.issueAvro.model.event.consumer;

import com.sngular.scsplugin.issueAvro.model.event.CreateOrder;

public interface ISubscribeOperationExternalAvro {

  void subscribeOperationExternalAvro(final CreateOrder value);
}