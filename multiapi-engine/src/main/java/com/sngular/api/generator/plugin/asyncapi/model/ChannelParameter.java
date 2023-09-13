package com.sngular.api.generator.plugin.asyncapi.model;

import lombok.Builder;
import lombok.Value;

@Value
@Builder
public class ChannelParameter {

  String name;

  String type;
}
