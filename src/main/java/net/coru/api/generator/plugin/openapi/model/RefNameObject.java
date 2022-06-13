package net.coru.api.generator.plugin.openapi.model;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RefNameObject {

  private String refName;

  private Boolean checkImport;
}
