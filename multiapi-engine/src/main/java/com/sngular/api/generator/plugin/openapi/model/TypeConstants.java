/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.Set;

public final class TypeConstants {

  private static final Set<String> BOOLEAN_VALUES = Set.of(Boolean.TRUE.toString(), Boolean.FALSE.toString());

  public static final String NUMBER = "number";

  public static final String BOOLEAN = "boolean";

  public static final String OBJECT = "object";

  public static final String ARRAY = "array";

  public static final String MAP = "map";

  public static final String BIG_DECIMAL = "bigDecimal";

  public static final String INTEGER = "integer";

  public static final String DOUBLE = "double";

  public static final String FLOAT = "float";

  public static final String LONG = "long";

  public static final String STRING = "string";

  public static final String ENUM = "enum";

  public static final String LOCALDATE = "localdate";

  public static final String LOCALDATETIME = "localdatetime";

  public static final String ZONEDDATE = "zoneddate";

  public static final String ZONEDDATETIME = "zoneddatetime";

  public static final String OFFSETDATE = "offsetdate";

  public static final String OFFSETDATETIME = "offsetdatetime";

  public static final String INT_32 = "int32";

  public static final String INT_64 = "int64";

  public static final Set<String> BASIC_OBJECT_TYPE = Set.of(NUMBER, STRING, BOOLEAN, INTEGER, ARRAY);

  public static final Set<String> NO_IMPORT_TYPE = Set.of(STRING, INTEGER, OBJECT);

  public static final Set<String> ALL_TYPES = Set.of(
      NUMBER,
      BOOLEAN,
      OBJECT,
      ARRAY,
      BIG_DECIMAL,
      INTEGER,
      DOUBLE,
      FLOAT,
      LONG,
      STRING,
      ENUM,
      LOCALDATE,
      LOCALDATETIME,
      ZONEDDATE,
      ZONEDDATETIME,
      OFFSETDATE,
      OFFSETDATETIME
  );

  public static boolean isBoolean(final String isBoolean) {
    return Boolean.parseBoolean(isBoolean);
  }
  
  public enum TimeType {
    LOCAL,
    ZONED,
    OFFSET
  }

  private TypeConstants() {

  }

}
