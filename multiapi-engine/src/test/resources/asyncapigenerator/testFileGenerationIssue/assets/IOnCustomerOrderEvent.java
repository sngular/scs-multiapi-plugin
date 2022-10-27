package net.coru.scsplugin.filegenerationissue.model.event.producer;

import net.coru.scsplugin.filegenerationissue.model.event.CustomerOrderEventMessageDTO;

public interface IOnCustomerOrderEvent {

  CustomerOrderEventMessageDTO onCustomerOrderEvent();
}