package com.github.issue.listener;

import java.util.function.Consumer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.github.issue.model.UserMessage;

@Configuration
public class Subscriber {

  private final IUserSignedUp userSignedUp;

  protected Subscriber(final IUserSignedUp userSignedUp) {
    this.userSignedUp = userSignedUp;
  }

  @Bean
  public Consumer<UserMessage> userSignedUp() {
    return value -> userSignedUp.userSignedUp(value);
  }


}
