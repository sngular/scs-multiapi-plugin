package com.corunet.api.generator.plugin.openapi;

import java.util.Set;

public class BasicTypeConstants {


  public static final String NUMBER = "number";

  public static final String STRING = "string";

  public static final String BOOLEAN = "boolean";

  public static final String INTEGER = "integer";

  public static final Set<String> BASIC_OBJECT_TYPE = Set.of(NUMBER, STRING, BOOLEAN, INTEGER
  );

  private BasicTypeConstants() {

  }

}
