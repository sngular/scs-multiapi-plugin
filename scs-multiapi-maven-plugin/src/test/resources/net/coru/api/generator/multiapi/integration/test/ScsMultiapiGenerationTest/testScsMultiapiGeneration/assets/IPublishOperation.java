package net.coru.scsplugin.business_model.model.event.consumer;

import net.coru.scsplugin.business_model.model.event.OrderCreatedDTO;

public interface IPublishOperation {

    void publishOperation(OrderCreatedDTO value);
}