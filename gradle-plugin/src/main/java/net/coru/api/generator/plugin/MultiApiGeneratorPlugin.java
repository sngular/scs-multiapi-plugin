package net.coru.api.generator.plugin;

import net.coru.api.generator.plugin.model.FileSpec;
import org.gradle.api.DefaultTask;
import org.gradle.api.Project;
import org.gradle.api.file.RegularFileProperty;
import org.gradle.api.provider.Property;
import org.gradle.api.tasks.Input;
import org.gradle.api.tasks.OutputDirectory;

public abstract class MultiApiGeneratorPlugin extends DefaultTask {

  @Input
  abstract Property<FileSpec> getFileSpec();

  @OutputDirectory
  public abstract RegularFileProperty getTargetFolder();

  public void apply(Project project) {

  }
}
