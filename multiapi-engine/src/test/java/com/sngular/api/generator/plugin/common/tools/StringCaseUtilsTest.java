package com.sngular.api.generator.plugin.common.tools;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Test;

public class StringCaseUtilsTest {

  @Test
  public void toCamelCase_basicAllOf() {
    assertEquals("AllOf", StringCaseUtils.toCamelCase("allof"));
    assertEquals("AllOf", StringCaseUtils.toCamelCase("AllOf"));
    assertEquals("AllOf", StringCaseUtils.toCamelCase("ALLOF"));
  }

  @Test
  public void toCamelCase_snakeToCamelWithAcronym() {
    assertEquals("ApiTestAllOfDTO", StringCaseUtils.toCamelCase("api_test_allof_dto"));
    assertEquals("ApiTestAllOfDTO", StringCaseUtils.toCamelCase("apiTest_allof_dto"));
  }

  @Test
  public void toCamelCase_preserveAcronyms() {
    assertEquals("MyDTO", StringCaseUtils.toCamelCase("my_dto"));
    assertEquals("MyID", StringCaseUtils.toCamelCase("my_id"));
  }

}

