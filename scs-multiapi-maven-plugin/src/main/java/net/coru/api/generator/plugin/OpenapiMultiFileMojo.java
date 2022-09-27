/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.plugin;

import java.io.File;
import java.nio.file.Path;
import java.util.List;

import lombok.extern.slf4j.Slf4j;
import net.coru.api.generator.plugin.exception.GeneratedSourceFolderException;
import net.coru.api.generator.plugin.openapi.OpenApiGenerator;
import net.coru.api.generator.plugin.openapi.parameter.SpecFile;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.apache.maven.project.MavenProject;

@Mojo(name = "openapi-generation", defaultPhase = LifecyclePhase.GENERATE_SOURCES, requiresDependencyResolution = ResolutionScope.COMPILE)
@Slf4j
public final class OpenapiMultiFileMojo extends AbstractMojo {

  @Parameter(defaultValue = "${project}", required = true, readonly = true)
  private MavenProject project;

  @Parameter(defaultValue = "${project.build.directory}", required = true, readonly = true)
  private File targetFolder;

  @Parameter(property = "specFiles")
  private List<SpecFile> specFiles;

  @Parameter(name = "overwriteModel", property = "overwriteModel", defaultValue = "true")
  private Boolean overwriteModel;

  @Parameter(name = "generatedSourcesFolder", property = "generatedSourcesFolder", defaultValue = PluginConstants.GENERATED_SOURCES_FOLDER)
  private String generatedSourcesFolder;

  private String processedGeneratedSourcesFolder;

  @Override
  public void execute() throws MojoExecutionException {
    processGeneratedSourcesFolderName();
    addGeneratedSourcesToProject();
    OpenApiGenerator openApiGenerator = new OpenApiGenerator(overwriteModel, processedGeneratedSourcesFolder, project.getModel().getGroupId(),
                                                                                           targetFolder, project.getBasedir());
    if (null != specFiles && !specFiles.isEmpty()) {
      openApiGenerator.processFileSpec(specFiles);
    } else {
      throw new MojoExecutionException("Code generation failed. Not exists FileSpec configuration to generate package and class");
    }

  }

  private void processGeneratedSourcesFolderName() {
    if (generatedSourcesFolder.matches("[a-zA-Z\\d\\-]+")) {
      processedGeneratedSourcesFolder = generatedSourcesFolder + "/" + PluginConstants.GENERATED_SOURCES_API_GENERATOR_FOLDER;
    } else {
      throw new GeneratedSourceFolderException("OpenApi", generatedSourcesFolder);
    }
  }

  private void addGeneratedSourcesToProject() {
    final Path projectPath = targetFolder.toPath().resolve(processedGeneratedSourcesFolder);
    project.addCompileSourceRoot(projectPath.toString());
  }



}
