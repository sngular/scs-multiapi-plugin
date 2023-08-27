package com.sngular.scsplugin.filegenerationissue.model.event.consumer;

import com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerEventPayloadDTO;

public interface IOnCustomerEvent {

  void onCustomerEvent(final CustomerEventPayloadDTO value);
}