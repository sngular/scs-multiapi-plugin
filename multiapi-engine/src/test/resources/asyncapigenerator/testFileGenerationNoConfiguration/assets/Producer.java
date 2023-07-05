package groupId;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.apigenerator.asyncapi.model.messages.OrderCreated;

@Configuration
public class Producer {

  private final IPublishOperationFileGenerationNoConf publishOperationFileGenerationNoConf;

  protected Producer(final IPublishOperationFileGenerationNoConf publishOperationFileGenerationNoConf) {
    this.publishOperationFileGenerationNoConf = publishOperationFileGenerationNoConf;
  }

  @Bean
  public Supplier<OrderCreated> publishOperationFileGenerationNoConf() {
    return () -> publishOperationFileGenerationNoConf.publishOperationFileGenerationNoConf();
  }


}
