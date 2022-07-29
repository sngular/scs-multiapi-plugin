package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationWithoutOperationIds.assets;

import net.coru.scsplugin.business_model.model.event.CreateOrderDTO;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrderDTO value);
}