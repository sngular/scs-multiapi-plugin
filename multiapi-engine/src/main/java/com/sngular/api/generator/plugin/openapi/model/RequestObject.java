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
public class RequestObject {

  private Boolean required;

  private List<ContentObject> contentObjects = new ArrayList<>();

  public static final class RequestObjectBuilder {

    private final List<ContentObject> contentObjects = new ArrayList<>();

    public RequestObjectBuilder contentObjects(final List<ContentObject> contentObjects) {
      this.contentObjects.addAll(contentObjects);
      return this;
    }

    public RequestObjectBuilder contentObject(final ContentObject contentObject) {
      this.contentObjects.add(contentObject);
      return this;
    }
  }
}
