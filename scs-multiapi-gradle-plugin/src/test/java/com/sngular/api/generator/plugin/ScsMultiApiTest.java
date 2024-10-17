/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package com.sngular.api.generator.plugin;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;

import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;

class ScsMultiApiTest {

  @Test
  void greeterPluginAddsOpenApiTaskToProject() {
    Project project = ProjectBuilder.builder().build();
    project.getPluginManager().apply("com.sngular.scs-multiapi-gradle-plugin");

    assertInstanceOf(OpenApiTask.class, project.getTasks().getByName("openApiTask"));
  }

  @Test
  void greeterPluginAddsAsyncApiTaskTaskToProject() {
    Project project = ProjectBuilder.builder().build();
    project.getPluginManager().apply("com.sngular.scs-multiapi-gradle-plugin");

    assertInstanceOf(AsyncApiTask.class, project.getTasks().getByName("asyncApiTask"));
  }

}
