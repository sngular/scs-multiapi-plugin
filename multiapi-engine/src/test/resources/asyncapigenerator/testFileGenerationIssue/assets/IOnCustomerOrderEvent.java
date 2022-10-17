package net.coru.scsplugin.business_model.model.event.producer;

import net.coru.scsplugin.business_model.model.event.CustomerOrderEventMessageDTO;

public interface IOnCustomerOrderEvent {

  CustomerOrderEventMessageDTO onCustomerOrderEvent();
}