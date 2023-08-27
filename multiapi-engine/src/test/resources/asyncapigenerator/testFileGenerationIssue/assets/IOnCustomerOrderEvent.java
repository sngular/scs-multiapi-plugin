package com.sngular.scsplugin.filegenerationissue.model.event.producer;

import com.sngular.scsplugin.filegenerationissue.model.event.schemas.CustomerOrderEventPayloadDTO;

public interface IOnCustomerOrderEvent {

  CustomerOrderEventPayloadDTO onCustomerOrderEvent();
}