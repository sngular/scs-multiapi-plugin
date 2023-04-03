package com.sngular.scsplugin.filegenerationissue.model.event.producer;

import com.sngular.scsplugin.filegenerationissue.model.event.messages.CustomerOrderEventMessageDTO;

public interface IOnCustomerOrderEvent {

  CustomerOrderEventMessageDTO onCustomerOrderEvent();
}