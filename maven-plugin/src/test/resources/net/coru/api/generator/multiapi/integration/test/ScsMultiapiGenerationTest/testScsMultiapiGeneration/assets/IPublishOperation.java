package net.coru.api.generator.multiapi.integration.test.ScsMultiapiGenerationTest.testScsMultiapiGeneration.assets;

import net.coru.scsplugin.business_model.model.event.OrderCreatedDTO;

public interface IPublishOperation {

    void publishOperation(OrderCreatedDTO value);
}