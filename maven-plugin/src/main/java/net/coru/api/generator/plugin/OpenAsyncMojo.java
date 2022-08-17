/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import net.coru.api.generator.plugin.asyncapi.AsyncApiGenerator;
import net.coru.api.generator.plugin.asyncapi.parameter.FileSpec;
import net.coru.api.generator.plugin.exception.GeneratedSourceFolderException;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

@SuppressWarnings("checkstyle:ClassDataAbstractionCoupling")
@Mojo(name = "asyncapi-generation", defaultPhase = LifecyclePhase.GENERATE_SOURCES, requiresDependencyResolution = ResolutionScope.COMPILE)
public final class OpenAsyncMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  private MavenProject project;

  @Parameter(defaultValue = "${project.build.directory}", required = true, readonly = true)
  private File targetFolder;

  @Parameter(property = "fileSpecs")
  private List<FileSpec> fileSpecs;

  @Parameter(name = "generatedSourcesFolder", property = "generatedSourcesFolder", defaultValue = PluginConstants.GENERATED_SOURCES_FOLDER)
  private String generatedSourcesFolder;

  @Override
  public void execute() {
    var processedGeneratedSourcesFolder = processGeneratedSourcesFolderName();
    addGeneratedSourcesToProject(processedGeneratedSourcesFolder);


    var asyncApiGenerator = new AsyncApiGenerator(targetFolder, processedGeneratedSourcesFolder, project.getModel().getGroupId(), project.getBasedir());

    asyncApiGenerator.processFileSpec(fileSpecs);

  }

  private String processGeneratedSourcesFolderName() {
    if (generatedSourcesFolder.matches("[a-zA-Z\\d\\-]+")) {
      return generatedSourcesFolder + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER;
    } else {
      throw new GeneratedSourceFolderException("Asyncapi", generatedSourcesFolder);
    }
  }

  private void addGeneratedSourcesToProject(final String processedGeneratedSourcesFolder) {
    final Path projectPath = targetFolder.toPath().resolve(processedGeneratedSourcesFolder);
    project.addCompileSourceRoot(projectPath.toString());
  }

}
