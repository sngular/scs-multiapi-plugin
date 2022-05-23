package com.corunet.api.generator.plugin.asyncapi.exception;

public class KafkaTopicSeparatorException extends RuntimeException {

  public KafkaTopicSeparatorException(final String channelName) {
    super("Channel name " + channelName +" includes a separator not allowed. Please use \".\" or \"-\" instead.");
  }
}
