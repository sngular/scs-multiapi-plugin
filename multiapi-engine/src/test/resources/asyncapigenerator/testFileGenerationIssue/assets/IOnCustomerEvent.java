package net.coru.scsplugin.business_model.model.event.consumer;

import net.coru.scsplugin.business_model.model.event.CustomerEventMessageDTO;

public interface IOnCustomerEvent {

  void onCustomerEvent(final CustomerEventMessageDTO value);
}