package net.coru.generator.multiapi.model.event.consumer;

import net.coru.generator.multiapi.model.event.OrderCreatedDTO;

public interface IPublishOperation {

    void publishOperation(OrderCreatedDTO value);
}