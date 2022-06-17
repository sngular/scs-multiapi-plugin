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
public class ResponseObject {

  private String responseName;

  private String description;

  private List<ContentObject> contentObjects = new ArrayList<>();

  public static class ResponseObjectBuilder {

    private List<ContentObject> contentObjects = new ArrayList<>();

    public ResponseObjectBuilder contentObjects(final List<ContentObject> contentObjects) {
      this.contentObjects.addAll(contentObjects);
      return this;
    }

    public ResponseObjectBuilder contentObject(final ContentObject contentObject) {
      this.contentObjects.add(contentObject);
      return this;
    }
  }
}
