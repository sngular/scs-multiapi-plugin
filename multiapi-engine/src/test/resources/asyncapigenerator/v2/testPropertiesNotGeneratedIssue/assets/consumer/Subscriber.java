package com.sngular.scsplugin.notgeneratedproperties.consumer;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.sngular.scsplugin.model.schemas.UserSignedUp;

@Configuration
public class Subscriber {

  private final IEmitUserSignUpEvent emitUserSignUpEvent;

  protected Subscriber(final IEmitUserSignUpEvent emitUserSignUpEvent) {
    this.emitUserSignUpEvent = emitUserSignUpEvent;
  }

  @Bean
  public Consumer<UserSignedUp> emitUserSignUpEvent() {
    return value -> emitUserSignUpEvent.emitUserSignUpEvent(value);
  }


}
