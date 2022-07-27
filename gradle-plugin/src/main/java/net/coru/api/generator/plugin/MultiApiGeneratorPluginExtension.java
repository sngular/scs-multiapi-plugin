package net.coru.api.generator.plugin;

public class MultiApiGeneratorPluginExtension {

  private String greeter = "MultiApiGenerator";

  protected String getGreeter() {
    return greeter;
  }

  protected void setGreeter(final String greeter) {
    this.greeter = greeter;
  }
}
