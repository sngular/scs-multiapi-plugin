package net.coru.scsplugin.filegenerationissue.model.event.consumer;

import net.coru.scsplugin.filegenerationissue.model.event.CustomerEventMessageDTO;

public interface IOnCustomerEvent {

  void onCustomerEvent(final CustomerEventMessageDTO value);
}