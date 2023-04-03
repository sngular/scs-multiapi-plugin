package com.sngular.scsplugin.filegenerationissue.model.event.consumer;

import com.sngular.scsplugin.filegenerationissue.model.event.messages.CustomerEventMessageDTO;

public interface IOnCustomerEvent {

  void onCustomerEvent(final CustomerEventMessageDTO value);
}