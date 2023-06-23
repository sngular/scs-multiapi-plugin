/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuthObject {

  private List<String> securityRequirements;

  public static class AuthObjectBuilder {

    private final List<String> securityRequirements = new ArrayList<>();

    public final AuthObjectBuilder securityRequirements(final List<String> securityRequirements) {
      this.securityRequirements.addAll(securityRequirements);
      return this;
    }

    public final AuthObjectBuilder securityRequirement(final String securityRequirement) {
      this.securityRequirements.add(securityRequirement);
      return this;
    }
  }
}
