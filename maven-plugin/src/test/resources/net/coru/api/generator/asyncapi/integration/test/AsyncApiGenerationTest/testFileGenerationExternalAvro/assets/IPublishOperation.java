package net.coru.scsplugin.business_model.model.event.producer;

import net.coru.scsplugin.business_model.model.event.Order;

public interface IPublishOperation {

    Order publishOperation();
}