package com.sngular.scsplugin.filegenerationissue.model.event.producer;

import com.sngular.scsplugin.filegenerationissue.model.event.CustomerOrderEventPayloadDTO;

public interface IOnCustomerOrderEvent {

  CustomerOrderEventPayloadDTO onCustomerOrderEvent();
}