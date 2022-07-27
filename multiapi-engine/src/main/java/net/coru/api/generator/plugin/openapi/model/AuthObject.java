/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuthObject {

  private List<String> securityRequirements = new ArrayList<>();

  public static class AuthObjectBuilder {

    private final List<String> securityRequirements = new ArrayList<>();

    public AuthObjectBuilder securityRequirements(final List<String> securityRequirements) {
      this.securityRequirements.addAll(securityRequirements);
      return this;
    }

    public AuthObjectBuilder securityRequirement(final String securityRequirement) {
      this.securityRequirements.add(securityRequirement);
      return this;
    }
  }
}
