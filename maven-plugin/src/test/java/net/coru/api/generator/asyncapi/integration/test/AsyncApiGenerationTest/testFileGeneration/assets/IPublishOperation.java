package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGeneration.assets;

import net.coru.scsplugin.business_model.model.event.OrderCreatedDTO;

public interface IPublishOperation {

    void publishOperation(OrderCreatedDTO value);
}