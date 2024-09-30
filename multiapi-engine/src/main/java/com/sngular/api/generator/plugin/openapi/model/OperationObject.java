/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.openapi.model;

import java.util.ArrayList;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

@Data
@Builder(toBuilder = true)
@AllArgsConstructor
public final class OperationObject {

  private String operationType;

  private String summary;

  private String operationId;

  private List<String> tags;

  private List<ResponseObject> responseObjects;

  private List<RequestObject> requestObjects;

  private List<ParameterObject> parameterObjects;

  private List<String> produces;

  private List<String> consumes;

  private List<String> securities;

  public static final class OperationObjectBuilder {

    private final List<String> tags = new ArrayList<>();

    private final List<ResponseObject> responseObjects = new ArrayList<>();

    private final List<RequestObject> requestObjects = new ArrayList<>();

    private final List<ParameterObject> parameterObjects = new ArrayList<>();

    private final List<String> produces = new ArrayList<>();

    private final List<String> consumes = new ArrayList<>();

    private final List<String> securities = new ArrayList<>();

    public OperationObjectBuilder tags(final List<String> tags) {
      this.tags.addAll(tags);
      return this;
    }

    public OperationObjectBuilder tag(final String tag) {
      this.tags.add(tag);
      return this;
    }

    public OperationObjectBuilder responseObjects(final List<ResponseObject> responseObjects) {
      this.responseObjects.addAll(responseObjects);
      return this;
    }

    public OperationObjectBuilder responseObject(final ResponseObject responseObject) {
      this.responseObjects.add(responseObject);
      return this;
    }

    public OperationObjectBuilder requestObjects(final List<RequestObject> requestObjects) {
      this.requestObjects.addAll(requestObjects);
      return this;
    }

    public OperationObjectBuilder requestObject(final RequestObject requestObject) {
      this.requestObjects.add(requestObject);
      return this;
    }

    public OperationObjectBuilder parameterObjects(final List<ParameterObject> parameterObjects) {
      this.parameterObjects.addAll(parameterObjects);
      return this;
    }

    public OperationObjectBuilder parameterObjects(final ParameterObject parameterObject) {
      this.parameterObjects.add(parameterObject);
      return this;
    }

    public OperationObjectBuilder produces(final List<String> produces) {
      this.produces.addAll(produces);
      return this;
    }

    public OperationObjectBuilder produce(final String produce) {
      this.produces.add(produce);
      return this;
    }

    public OperationObjectBuilder consumes(final List<String> consumes) {
      this.consumes.addAll(consumes);
      return this;
    }

    public OperationObjectBuilder consume(final String consume) {
      this.consumes.add(consume);
      return this;
    }

    public OperationObjectBuilder securities(final List<String> security) {
      this.securities.addAll(security);
      return this;
    }

    public OperationObjectBuilder security(final String security) {
      this.securities.add(security);
      return this;
    }
  }

}
