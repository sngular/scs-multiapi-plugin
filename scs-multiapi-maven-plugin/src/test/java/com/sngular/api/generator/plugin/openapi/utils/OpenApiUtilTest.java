/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.utils;

import static org.assertj.core.api.Assertions.assertThat;

import com.sngular.api.generator.plugin.openapi.utils.fixtures.OpenApiUtilTestHelper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class OpenApiUtilTest {

  @Test
  @DisplayName("Test the Java File Generation")
  void testProcessJavaFileName() {
    String javaFileNameWithBars = OpenApiUtil.processJavaFileName(OpenApiUtilTestHelper.PROCESSSJAVAFILENAMEWITHBARS);
    String javaFileName = OpenApiUtil.processJavaFileName(OpenApiUtilTestHelper.PROCESSJAVAFILENAME);
    assertThat(javaFileNameWithBars).hasToString(OpenApiUtilTestHelper.TESTNAME);
    assertThat(javaFileName).hasToString(OpenApiUtilTestHelper.TESTNAME);
  }
}
