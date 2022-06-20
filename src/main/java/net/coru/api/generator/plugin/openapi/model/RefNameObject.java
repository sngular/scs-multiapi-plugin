package net.coru.api.generator.plugin.openapi.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RefNameObject {

  private String refName;

  private Boolean checkImport;
}
