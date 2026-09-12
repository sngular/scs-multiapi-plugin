/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi.v2;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

import com.sngular.api.generator.plugin.asyncapi.AsyncApiGenerator;
import com.sngular.api.generator.plugin.asyncapi.exception.InvalidAvroException;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.plugin.exception.InvalidAPIException;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.CleanupMode;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

class AsyncApiGeneratorTest {

  private static final int SPRING_BOOT_VERSION = 2;
  @TempDir(cleanup = CleanupMode.NEVER)
  static Path baseDir;
  private static AsyncApiGenerator asyncApiGenerator;

  @BeforeAll
  static void setup() {
    asyncApiGenerator =
        new AsyncApiGenerator(SPRING_BOOT_VERSION,
            true,
            new File(baseDir.toAbsolutePath() + File.separator + AsyncApiGeneratorFixtures.TARGET),
            AsyncApiGeneratorFixtures.GENERATED,
            "groupId",
            Path.of("src", "test", "resources").toFile()
        );
  }

  static Stream<Arguments> fileSpecToProcess() {
    return Stream.of(
        Arguments.of("TestFileGeneration", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION, AsyncApiGeneratorFixtures.validateTestFileGeneration()),
        Arguments.of("TestIssueGeneration", AsyncApiGeneratorFixtures.TEST_ISSUE_GENERATION, AsyncApiGeneratorFixtures.validateTestIssueGeneration()),
        Arguments.of("TestReservedWordsGeneration", AsyncApiGeneratorFixtures.TEST_RESERVED_WORDS_GENERATION, AsyncApiGeneratorFixtures.validateTestReservedWordsGeneration()),
        Arguments.of("TestRareCharsGeneration", AsyncApiGeneratorFixtures.TEST_RARE_CHARS_GENERATION, AsyncApiGeneratorFixtures.validateTestRareCharsGeneration()),
        Arguments.of("TestFileGenerationIssue", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_ISSUE, AsyncApiGeneratorFixtures.validateTestFileGenerationIssue()),
        Arguments.of("TestFileGenerationExternalAvro", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_EXTERNAL_AVRO,
            AsyncApiGeneratorFixtures.validateTestFileGenerationExternalAvro()),
        Arguments.of("TestFileGenerationStreamBridge", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_STREAM_BRIDGE,
            AsyncApiGeneratorFixtures.validateTestFileGenerationStreamBridge()),
        Arguments.of("TestFileGenerationWithoutIds", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITHOUT_OPERATION_IDS,
            AsyncApiGeneratorFixtures.validateTestFileGenerationWithoutOperationIds()),
        Arguments.of("TestFileGenerationArrayString", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITH_ARRAY_STRING,
            AsyncApiGeneratorFixtures.validateTestFileGenerationArrayString()),
        Arguments.of("TestIssueGenerateSupplier", AsyncApiGeneratorFixtures.TEST_ISSUE_GENERATE_SUPPLIER,
            AsyncApiGeneratorFixtures.validateTestIssueGenerateSupplier()),
        Arguments.of("TestIssueSimpleTypeGenerate", AsyncApiGeneratorFixtures.TEST_ISSUE_SIMPLE_TYPE_GENERATION,
            AsyncApiGeneratorFixtures.validateTestIssueSimpleTypeGeneration()),
        Arguments.of("TestIssueInfiniteLoop", AsyncApiGeneratorFixtures.TEST_ISSUE_INFINITE_LOOP,
            AsyncApiGeneratorFixtures.validateTestIssueInfiniteLoop()),
        Arguments.of("TestCustomValidators", AsyncApiGeneratorFixtures.TEST_CUSTOM_VALIDATORS, AsyncApiGeneratorFixtures.validateCustomValidators(SPRING_BOOT_VERSION)),
        Arguments.of("TestCustomValidatorsDifferentPackages", AsyncApiGeneratorFixtures.TEST_CUSTOM_VALIDATORS_DIFFERENT_PACKAGES,
            AsyncApiGeneratorFixtures.validateCustomValidatorsDifferentPackages()),
        Arguments.of("TestIssue248CustomValidators", AsyncApiGeneratorFixtures.TEST_ISSUE_248_GENERATION,
            AsyncApiGeneratorFixtures.validateIssue248PackageFolderAlignment()),
        Arguments.of("TestModelClassExceptionGeneration", AsyncApiGeneratorFixtures.TEST_MODEL_CLASS_EXCEPTION_GENERATION,
            AsyncApiGeneratorFixtures.validateTestModelClassExceptionGeneration()),
        Arguments.of("TestNoSchemas", AsyncApiGeneratorFixtures.TEST_NO_SCHEMAS, AsyncApiGeneratorFixtures.validateNoSchemas()),
        Arguments.of("TestMessageNaming", AsyncApiGeneratorFixtures.TEST_MESSAGE_NAMING, AsyncApiGeneratorFixtures.validateMessageNaming()),
        Arguments.of("TestNestedObjectIssue", AsyncApiGeneratorFixtures.TEST_NESTED_OBJECT_ISSUE, AsyncApiGeneratorFixtures.validateNestedObject()),
        Arguments.of("TestConstantGeneration", AsyncApiGeneratorFixtures.TEST_CONSTANT_GENERATION, AsyncApiGeneratorFixtures.validateConstantGeneration()),
        Arguments.of("testPropertiesNotGeneratedIssue", AsyncApiGeneratorFixtures.PROPERTIES_NOT_GENERATED_ISSUE, AsyncApiGeneratorFixtures.validateNotGeneratedPropertiesIssue()),
        Arguments.of("TestFileGenerationWithKafkaBindings", AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_WITH_KAFKA_BINDINGS,
            AsyncApiGeneratorFixtures.validateTestFileGenerationWithKafkaBindings()),
        Arguments.of("TestSubObjectSameName", AsyncApiGeneratorFixtures.TEST_SUB_OBJECT_SAME_NAME,
            AsyncApiGeneratorFixtures.validateTestSubObjectSameName()),
        Arguments.of("TestReferenceFromLocalIssue", AsyncApiGeneratorFixtures.TEST_REFERENCE_FROM_LOCAL_ISSUE,
            AsyncApiGeneratorFixtures.validateTestReferenceFromLocalIssue()),
        Arguments.of("TestIssue292Streetlights", AsyncApiGeneratorFixtures.TEST_ISSUE_292_STREETLIGHTS,
            AsyncApiGeneratorFixtures.validateTestIssue292Streetlights()));
  }

  @ParameterizedTest(name = "Test {index} - Process File Spec for case {0}")
  @MethodSource("fileSpecToProcess")
  void processFileSpecForTestFileGeneration(final String type, final List<SpecFile> specFileList, final Function<Path, Boolean> validation) {
    asyncApiGenerator.processFileSpec(specFileList);
    Assertions.assertThat(validation.apply(baseDir)).isTrue();
  }

  @Test
  void testGenerateSpringwolfAnnotations() throws IOException {
    asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_GENERATE_SPRINGWOLF);

    final Path target = Path.of(baseDir.toString(), "target", "generated/com/sngular/scsplugin/springwolf/model/event");

    final Path subscriber = target.resolve("consumer/Subscriber.java");
    final Path producer = target.resolve("producer/Producer.java");

    Assertions.assertThat(subscriber).exists();
    Assertions.assertThat(producer).exists();

    final String subscriberContent = Files.readString(subscriber);
    Assertions.assertThat(subscriberContent)
        .contains("import io.github.stavshamir.springwolf.asyncapi.annotations.AsyncListener;")
        .contains("import io.github.stavshamir.springwolf.asyncapi.annotations.AsyncOperation;")
        .contains("@AsyncListener(operation = @AsyncOperation(channelName = \"order.created\", operationId = \"publishOperationFileGeneration\"))");

    final String producerContent = Files.readString(producer);
    Assertions.assertThat(producerContent)
        .contains("import io.github.stavshamir.springwolf.asyncapi.annotations.AsyncPublisher;")
        .contains("import io.github.stavshamir.springwolf.asyncapi.annotations.AsyncOperation;")
        .contains("@AsyncPublisher(operation = @AsyncOperation(channelName = \"order.createCommand\", operationId = \"subscribeOperationFileGeneration\"))");
  }

  @Test
  void testExceptionForTestGenerationWithNoOperationId() {
    Assertions.assertThatThrownBy(() -> asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_GENERATION_WITH_NO_OPERATION_ID)).isInstanceOf(InvalidAPIException.class);
  }

  @Test
  void testGenerateModelOnly() throws IOException {
    asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_GENERATE_MODEL_ONLY);

    final Path target = Path.of(baseDir.toString(), "target");
    final Path modelFolder = target.resolve("generated/com/sngular/scsplugin/modelonly/model/event");
    final Path consumerApiFolder = target.resolve("generated/com/sngular/scsplugin/modelonly/model/event/consumer");
    final Path producerApiFolder = target.resolve("generated/com/sngular/scsplugin/modelonly/model/event/producer");

    Assertions.assertThat(modelFolder).isNotEmptyDirectory();
    Assertions.assertThat(modelFolder.resolve("OrderDTO.java")).exists();
    Assertions.assertThat(modelFolder.resolve("OrderLineDTO.java")).exists();
    Assertions.assertThat(modelFolder.resolve("OrderProductDTO.java")).exists();
    Assertions.assertThat(modelFolder.resolve("WaiterDTO.java")).exists();

    Assertions.assertThat(consumerApiFolder).isDirectory();
    Assertions.assertThat(consumerApiFolder).isEmptyDirectory();
    Assertions.assertThat(producerApiFolder).isDirectory();
    Assertions.assertThat(producerApiFolder).isEmptyDirectory();

    final Path modelOnlyTree = target.resolve("generated/com/sngular/scsplugin/modelonly");
    try (Stream<Path> files = Files.walk(modelOnlyTree)) {
      Assertions.assertThat(files.map(Path::getFileName).map(Path::toString))
          .noneMatch(name -> name.startsWith("I") && name.endsWith(".java"))
          .noneMatch("Channels.java"::equals)
          .noneMatch("Subscriber.java"::equals)
          .noneMatch("Producer.java"::equals);
    }
  }

  @Test
  void testExceptionForTestGenerationWithNoOperationConfiguration() {
    Assertions.assertThatThrownBy(() -> asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_FILE_GENERATION_NO_CONFIG)).isInstanceOf(InvalidAPIException.class);
  }

  @Test
  void testExceptionForTestIssueInvalidAvro() {
    Assertions.assertThatThrownBy(() -> asyncApiGenerator.processFileSpec(AsyncApiGeneratorFixtures.TEST_ISSUE_INVALID_AVRO)).isInstanceOf(InvalidAvroException.class);
  }

}
