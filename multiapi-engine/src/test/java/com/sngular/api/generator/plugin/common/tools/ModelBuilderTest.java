package com.sngular.api.generator.plugin.common.tools;

import static org.assertj.core.api.Assertions.assertThat;

import java.nio.file.Path;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sngular.api.generator.plugin.common.model.CommonSpecFile;
import com.sngular.api.generator.plugin.common.model.SchemaFieldObject;
import com.sngular.api.generator.plugin.common.model.SchemaObject;
import org.junit.jupiter.api.Test;

public class ModelBuilderTest {

  @Test
  void testEnumNormalizationWithHyphen() throws Exception {
    final ObjectMapper mapper = new ObjectMapper();
    final String json = "{\"enum\":[\"in-course\",\"other-value\"]}";
    final JsonNode node = mapper.readTree(json);

    final Map<String, JsonNode> totalSchemas = new HashMap<>();
    final Set<String> antiLoop = new HashSet<>();
    final Map<String, SchemaObject> composited = new HashMap<>();

    final CommonSpecFile specFile = CommonSpecFile.builder().modelPackage("com.sngular.test").build();

    final SchemaObject schemaObject = ModelBuilder.buildSchemaObject(totalSchemas, "Status", node, antiLoop, composited, "parent", specFile, Path.of("."));

    // For enum schemas ModelBuilder stores the enum field inside the SchemaObject field list
    final Optional<SchemaFieldObject> enumFieldOptional = schemaObject.getFieldObjectList().stream().findFirst();
    assertThat(enumFieldOptional).as("Enum field should be present").isPresent();
    final SchemaFieldObject enumField = enumFieldOptional.get();

    final Map<String, String> enumValues = enumField.getEnumValues();

    // The value "in-course" must produce a constant named IN_COURSE
    assertThat(enumValues).containsKey("IN_COURSE");
    assertThat(enumValues).containsEntry("IN_COURSE", '"' + "in-course" + '"');
  }
}
