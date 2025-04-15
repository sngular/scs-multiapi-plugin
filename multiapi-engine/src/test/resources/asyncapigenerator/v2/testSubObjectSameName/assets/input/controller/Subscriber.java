package input.controller;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import input.model.Input;

@Configuration
public class Subscriber {

  private final IInput input;

  protected Subscriber(final IInput input) {
    this.input = input;
  }

  @Bean
  public Consumer<Input> input() {
    return value -> input.input(value);
  }


}
