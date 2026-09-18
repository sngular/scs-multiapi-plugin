/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.common.tools;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class StringCaseUtilsTest {

  @Test
  void aNameThatIsAlreadyAJavaIdentifierIsKept() {
    assertThat(StringCaseUtils.toJavaIdentifier("shipmentId")).isEqualTo("shipmentId");
    assertThat(StringCaseUtils.toJavaIdentifier("shipment_id")).isEqualTo("shipment_id");
    assertThat(StringCaseUtils.toJavaIdentifier("Authorization")).isEqualTo("Authorization");
  }

  @Test
  void separatorsAreFoldedIntoCamelCase() {
    assertThat(StringCaseUtils.toJavaIdentifier("Idempotency-Key")).isEqualTo("idempotencyKey");
    assertThat(StringCaseUtils.toJavaIdentifier("sort-by")).isEqualTo("sortBy");
    assertThat(StringCaseUtils.toJavaIdentifier("delivery.status")).isEqualTo("deliveryStatus");
    assertThat(StringCaseUtils.toJavaIdentifier("X-Request-ID")).isEqualTo("xRequestID");
    assertThat(StringCaseUtils.toJavaIdentifier("client ref")).isEqualTo("clientRef");
  }

  @Test
  void anIdentifierCannotStartWithADigit() {
    assertThat(StringCaseUtils.toJavaIdentifier("2fa-token")).isEqualTo("_2faToken");
  }

  @Test
  void aNameWithNothingToBuildAnIdentifierFromStillYieldsOne() {
    final String identifier = StringCaseUtils.toJavaIdentifier("-.-");
    assertThat(identifier).isNotEmpty();
    assertThat(Character.isJavaIdentifierStart(identifier.charAt(0))).isTrue();
    assertThat(StringCaseUtils.toJavaIdentifier("")).isEmpty();
    assertThat(StringCaseUtils.toJavaIdentifier(null)).isNull();
  }

  @Test
  void aReservedWordIsKeptAsAnIdentifierAndPrefixedAsAVariable() {
    assertThat(StringCaseUtils.toJavaIdentifier("new")).isEqualTo("new");
    assertThat(StringCaseUtils.toJavaVariableName("new")).isEqualTo("_new");
    assertThat(StringCaseUtils.toJavaVariableName("new-")).isEqualTo("_new");
    assertThat(StringCaseUtils.toJavaVariableName("if-match")).isEqualTo("ifMatch");
    assertThat(StringCaseUtils.toJavaVariableName("Idempotency-Key")).isEqualTo("idempotencyKey");
  }
}
