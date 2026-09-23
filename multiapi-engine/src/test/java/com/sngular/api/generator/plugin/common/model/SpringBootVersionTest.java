package com.sngular.api.generator.plugin.common.model;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.sngular.api.generator.plugin.openapi.exception.CodeGenerationException;
import org.junit.jupiter.api.Test;

class SpringBootVersionTest {

  @Test
  void aBareMajorIsTheLowestMinorOfThatLine() {
    // `3` keeps meaning what it meant before minors were accepted: nothing that needs 3.2 is generated for it.
    assertThat(SpringBootVersion.parse("3")).hasToString("3.0");
    assertThat(SpringBootVersion.parse("3").isAtLeast(3, 2)).isFalse();
    assertThat(SpringBootVersion.of(4)).hasToString("4.0");
  }

  @Test
  void acceptsMajorMinorAndIgnoresThePatch() {
    assertThat(SpringBootVersion.parse("3.2").isAtLeast(3, 2)).isTrue();
    assertThat(SpringBootVersion.parse("3.1").isAtLeast(3, 2)).isFalse();
    assertThat(SpringBootVersion.parse(" 4.0.8 ")).hasToString("4.0");
    assertThat(SpringBootVersion.parse("4").isAtLeast(3, 2)).isTrue();
  }

  @Test
  void blankMeansThePluginDefault() {
    assertThat(SpringBootVersion.parse(null)).hasToString("2.0");
    assertThat(SpringBootVersion.parse("")).hasToString("2.0");
    assertThat(SpringBootVersion.of(null)).hasToString("2.0");
  }

  @Test
  void rejectsWhatIsNotAVersion() {
    assertThatThrownBy(() -> SpringBootVersion.parse("three"))
        .isInstanceOf(CodeGenerationException.class)
        .hasMessageContaining("MAJOR.MINOR");
  }
}
