package net.coru.api.generator.plugin;

import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.file.RegularFileProperty;
import org.gradle.api.tasks.OutputDirectory;

public abstract class MultiApiGeneratorPlugin extends DefaultTask {



  @OutputDirectory
  public abstract RegularFileProperty getTargetFolder();

  public void apply(Project project) {

  }
}
