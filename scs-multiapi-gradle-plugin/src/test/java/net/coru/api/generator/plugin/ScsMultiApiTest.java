/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */
package net.coru.api.generator.plugin;

import static org.junit.jupiter.api.Assertions.assertTrue;

import org.gradle.api.Project;
import org.gradle.testfixtures.ProjectBuilder;
import org.junit.jupiter.api.Test;

class ScsMultiApiTest {

  @Test
  void greeterPluginAddsOpenApiTaskToProject() {
    Project project = ProjectBuilder.builder().build();
    project.getPluginManager().apply("net.coru.scs-multiapi-gradle-plugin");

    assertTrue(project.getTasks().getByName("openApiTask") instanceof OpenApiTask);
  }

  @Test
  void greeterPluginAddsGreetingTaskToProject() {
    Project project = ProjectBuilder.builder().build();
    project.getPluginManager().apply("net.coru.scs-multiapi-gradle-plugin");

    assertTrue(project.getTasks().getByName("asyncApiTask") instanceof OpenApiTask);
  }

}
