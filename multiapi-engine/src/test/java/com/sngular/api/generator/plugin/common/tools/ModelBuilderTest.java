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

  @Test
  void testArrayOfPropertylessAnyOfBecomesObjectList() throws Exception {
    // `items: {anyOf: [{type: object}]}` describes free-form elements: no member declares a
    // property, so there is nothing to model and the element type must degrade to Object instead
    // of an empty class whose single field would have no name to render.
    final String json = """
        {"type": "object",
         "properties": {
           "items": {"type": "array", "items": {"anyOf": [{"type": "object"}]}}
         }}""";

    final SchemaObject schemaObject = buildSchema("Pagination", json);

    assertThat(schemaObject.getFieldObjectList()).hasSize(1);
    final SchemaFieldObject items = schemaObject.getFieldObjectList().iterator().next();
    assertThat(items.getBaseName()).isEqualTo("items");
    assertThat(items.getDataType()).hasToString("List<Object>");
  }

  @Test
  void testPropertylessSchemaYieldsNoNamelessField() throws Exception {
    // A schema with neither properties nor a name of its own (the bare `{"type": "object"}` member
    // of an anyOf) contributes no field: every template interpolates the field name, so a nameless
    // field fails template processing.
    final SchemaObject schemaObject = buildSchema("Items", "{\"anyOf\": [{\"type\": \"object\"}]}");

    assertThat(schemaObject.getFieldObjectList()).isEmpty();
  }

  @Test
  void testPropertylessNamedSchemaKeepsItsOwnNameAsField() throws Exception {
    // A component schema that is a bare type has no property name to borrow other than its own.
    final SchemaObject schemaObject = buildSchema("Reference", "{\"type\": \"string\"}");

    assertThat(schemaObject.getFieldObjectList()).hasSize(1);
    assertThat(schemaObject.getFieldObjectList().iterator().next().getBaseName()).isEqualTo("Reference");
  }

  private SchemaObject buildSchema(final String className, final String json) throws Exception {
    final JsonNode node = new ObjectMapper().readTree(json);
    final CommonSpecFile specFile = CommonSpecFile.builder().modelPackage("com.sngular.test").build();

    return ModelBuilder.buildSchemaObject(new HashMap<>(), className, node, new HashSet<>(), new HashMap<>(), "parent", specFile, Path.of("."));
  }

  @Test
  void testAllOfMemberNarrowingAFreeFormPropertyWins() throws Exception {
    // `allOf` means the value satisfies every member, so when a paged wrapper redeclares the
    // free-form `items` it inherits, the typed declaration is the accurate one — whichever order
    // the members are written in.
    final String freeForm = """
        {"type": "object", "properties": {"items": {"type": "array", "items": {"anyOf": [{"type": "object"}]}}}}""";
    final String typed = """
        {"type": "object", "properties": {"items": {"type": "array", "items": {"type": "string"}}}}""";

    assertThat(itemsTypeOfAllOf(freeForm, typed)).hasToString("List<String>");
    assertThat(itemsTypeOfAllOf(typed, freeForm)).hasToString("List<String>");
  }

  private Object itemsTypeOfAllOf(final String firstMember, final String secondMember) throws Exception {
    final SchemaObject schemaObject = buildSchema("PaginatedThing", "{\"allOf\": [" + firstMember + ", " + secondMember + "]}");

    return schemaObject.getFieldObjectList().stream()
                       .filter(field -> "items".equals(field.getBaseName()))
                       .findFirst()
                       .orElseThrow()
                       .getDataType();
  }
}
