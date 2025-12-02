package com.sngular.api.generator.plugin.common.tools;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

public class TemplateUtilsTest {

  @Test
  void toLowerCamel_convertsSnakeToLowerCamel() {
    final TemplateUtils utils = new TemplateUtils();
    assertThat(utils.toLowerCamel("client_id")).isEqualTo("clientId");
    assertThat(utils.toLowerCamel("user_adress")).isEqualTo("userAdress");
    assertThat(utils.toLowerCamel("alreadyCamel")).isEqualTo("alreadyCamel");
    assertThat(utils.toLowerCamel(null)).isNull();
  }
}
