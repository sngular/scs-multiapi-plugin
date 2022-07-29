package net.coru.api.generator.asyncapi.integration.test.AsyncApiGenerationTest.testFileGenerationTwoYmlFiles.assets;

import java.util.function.Consumer;

import net.coru.apigenerator.asyncapi.model.CreateOrder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class Subscriber {

    private final ISubscribeOperation subscribeOperation;

    protected Subscriber(final ISubscribeOperation subscribeOperation){
      this.subscribeOperation = subscribeOperation;
    }

    @Bean
    public Consumer<CreateOrder> subscribeOperation(){ return value -> subscribeOperation.subscribeOperation(value); }


}
