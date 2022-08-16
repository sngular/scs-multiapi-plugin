package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationExternalAvro.assets;

import net.coru.scsplugin.business_model.model.event.CreateOrder;

public interface ISubscribeOperation {

    void subscribeOperation(CreateOrder value);
}