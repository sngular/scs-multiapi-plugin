/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.utils;

import static net.coru.api.generator.plugin.openapi.utils.fixtures.OpenApiUtilFixtures.TESTNAME;
import static org.assertj.core.api.Assertions.assertThat;

import net.coru.api.generator.plugin.openapi.utils.fixtures.OpenApiUtilFixtures;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class OpenApiUtilTest {

  @Test
  @DisplayName("Test the Java File Generation")
  void testProcessJavaFileName() {
    String javaFileNameWithBars = OpenApiUtil.processJavaFileName(OpenApiUtilFixtures.PROCESSSJAVAFILENAMEWITHBARS);
    String javaFileName = OpenApiUtil.processJavaFileName(OpenApiUtilFixtures.PROCESSJAVAFILENAME);
    assertThat(javaFileNameWithBars).hasToString(TESTNAME);
    assertThat(javaFileName).hasToString(TESTNAME);
  }
}
