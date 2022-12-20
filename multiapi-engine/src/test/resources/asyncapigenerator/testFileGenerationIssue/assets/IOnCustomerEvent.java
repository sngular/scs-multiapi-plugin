package com.sngular.scsplugin.filegenerationissue.model.event.consumer;

import com.sngular.scsplugin.filegenerationissue.model.event.CustomerEventMessageDTO;

public interface IOnCustomerEvent {

  void onCustomerEvent(final CustomerEventMessageDTO value);
}