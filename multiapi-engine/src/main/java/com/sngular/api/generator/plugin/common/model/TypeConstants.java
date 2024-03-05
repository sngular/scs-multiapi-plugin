/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.model;

import java.util.Set;

public final class TypeConstants {

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

  public static final String DATE = "date";

  public static final String DATE_TIME = "date-time";

  public static final String LOCAL_DATE = "localdate";

  public static final String LOCAL_DATE_TIME = "localdatetime";

  public static final String ZONED_DATE = "zoneddate";

  public static final String ZONED_DATE_TIME = "zoneddatetime";

  public static final String OFFSET_DATE = "offsetdate";

  public static final String OFFSET_DATE_TIME = "offsetdatetime";

  public static final String INT_32 = "int32";

  public static final String INT_64 = "int64";

  public static final Set<String> BASIC_OBJECT_TYPE = Set.of(NUMBER, STRING, BOOLEAN, INTEGER, ARRAY);

  public static final Set<String> NO_IMPORT_TYPE = Set.of(STRING, INTEGER, OBJECT);

  public static final Set<String> NO_PROCESS_TYPE = Set.of(NUMBER,
                                                           BOOLEAN,
                                                           BIG_DECIMAL,
                                                           INTEGER,
                                                           DOUBLE,
                                                           FLOAT,
                                                           LONG,
                                                           STRING,
                                                           ENUM,
                                                           LOCAL_DATE,
                                                           LOCAL_DATE_TIME,
                                                           ZONED_DATE,
                                                           ZONED_DATE_TIME,
                                                           OFFSET_DATE,
                                                           OFFSET_DATE_TIME);

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
      LOCAL_DATE,
      LOCAL_DATE_TIME,
      ZONED_DATE,
      ZONED_DATE_TIME,
      OFFSET_DATE,
      OFFSET_DATE_TIME
  );

  private TypeConstants() {
  }

  public static boolean isBoolean(final String isBoolean) {
    return Boolean.parseBoolean(isBoolean.toLowerCase());
  }

  public enum TimeType {
    LOCAL,
    ZONED,
    OFFSET

  }
}
