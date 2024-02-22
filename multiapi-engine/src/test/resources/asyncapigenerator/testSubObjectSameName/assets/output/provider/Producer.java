package output.provider;

import java.util.function.Supplier;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import output.model.Output;

@Configuration
public class Producer {

  private final IOutput output;

  protected Producer(final IOutput output) {
    this.output = output;
  }

  @Bean
  public Supplier<Output> output() {
    return () -> output.output();
  }


}
