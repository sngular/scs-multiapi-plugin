/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.parameter;

import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;

@Data
@SuperBuilder(toBuilder = true)
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class SpecFile extends CommonSpecFile {

  private String clientPackage;

  private boolean callMode;

  private boolean useTagsGroup;

  private boolean isReactive;

  /**
   * With {@code callMode}, whether the generated {@code *Api} client classes are Spring {@code @Component}s. Unset means
   * {@code true}, as before; {@code false} leaves declaring them, with a configured client, to the service. The reactive
   * client classes never were components.
   */
  private Boolean clientComponent;

  /**
   * With {@code callMode}, generates each {@code *Api} as a Spring HTTP service interface ({@code @HttpExchange}) instead of a
   * client class, to be backed by the service's own {@code RestClient}/{@code WebClient} through
   * {@code HttpServiceProxyFactory} (or {@code @ImportHttpServices} on Spring Boot 4). Requires Spring Boot 3 or later.
   */
  private boolean useHttpExchange;

  /**
   * Declares every generated Java name in camel case ({@code page_num} as {@code pageNum}, {@code getPageNum()}), while the
   * contract names keep being the ones sent and bound ({@code @JsonProperty}, {@code @RequestParam(name = ...)}). Defaults to
   * {@code false}: Java names are the contract names whenever they are legal identifiers.
   */
  private boolean useCamelCaseNames;

  public boolean shouldRegisterClientComponent() {
    return !Boolean.FALSE.equals(clientComponent);
  }
}

