package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationWithoutOperationIds.assets;

import java.util.function.Consumer;

import net.coru.scsplugin.business_model.model.event.CreateOrderDTO;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class TestClassName {

    private final ISubscribeOperation subscribeOperation;

    protected TestClassName(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrderDTO> subscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }


}
