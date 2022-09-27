package net.coru.api.generator.plugin.asyncapi;

import static net.coru.api.generator.test.utils.TestUtils.validateFiles;
import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

import net.coru.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import net.coru.api.generator.plugin.asyncapi.parameter.SpecFile;

public class AsyncApiGeneratorFixtures {

  final static List<SpecFile> TEST_FILE_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGeneration/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("publishOperationFileGeneration")
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.business_model.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.business_model.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("subscribeOperationFileGeneration")
                                            .modelNameSuffix("Mapper")
                                            .apiPackage("net.coru.scsplugin.business_model.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.business_model.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_EXTERNAL_AVRO = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationExternalAvro/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("subscribeOperationExternalAvro")
                                            .apiPackage("net.coru.scsplugin.externalAvro.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.externalAvro.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("publishOperationExternalAvro")
                                            .apiPackage("net.coru.scsplugin.externalAvro.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.externalAvro.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_STREAM_BRIDGE = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationStreamBridge/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("subscribeOperationStreamBridge")
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.streambridge.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.streambridge.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .ids("publishOperationStreamBridge")
                                                .apiPackage("net.coru.scsplugin.streambridge.model.event.producer")
                                                .modelPackage("net.coru.scsplugin.streambridge.model.event")
                                                .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_WITHOUT_IDS = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationWithOutOperationIds/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.withOutIds.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.withOutIds.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .apiPackage("net.coru.scsplugin.withOutIds.model.event.producer")
                                                .modelPackage("net.coru.scsplugin.withOutIds.model.event")
                                                .modelNameSuffix("Mapper")
                                                .build())
          .build()
  );

  final static String TARGET = "target";

  final static String GENERATED = "generated/";

  static Function<Path, Boolean> VALIDATE_TEST_FILE_GENERATION() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/business_model/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/business_model/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGeneration/assets/TestClassName.java");

    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGeneration/assets/Producer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> VALIDATE_TEST_FILE_GENERATION_EXTERNAL_AVRO() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/externalAvro/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/externalAvro/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Subscriber.java");

    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Producer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> VALIDATE_TEST_FILE_GENERATION_STREAM_BRIDGE() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/streambridge/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/streambridge/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationStreamBridge/assets/TestClassName.java");
    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/StreamBridgeProducer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> VALIDATE_TEST_FILE_GENERATION_WITHOUT_IDS() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/withOutIds/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/withOutIds/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/TestClassName.java");
    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/StreamBridgeProducer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  private static Boolean commonTest(
      final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetConsumer, final String targetProducer) {
    Boolean result = Boolean.TRUE;
    try {
      Path pathToTarget = Path.of(resultPath.toString(), "target");
      Path pathToTargetConsumer = pathToTarget.resolve(targetConsumer);

      File targetConsumerFolder = pathToTargetConsumer.toFile();
      assertThat(targetConsumerFolder).isNotEmptyDirectory();
      validateFiles(expectedFile, targetConsumerFolder);

      if (!expectedModelFiles.isEmpty()) {
        Path pathToTargetProducer = pathToTarget.resolve(targetProducer);
        File targetProducerFolder = pathToTargetProducer.toFile();
        assertThat(targetProducerFolder).isNotEmptyDirectory();
        validateFiles(expectedModelFiles, targetProducerFolder);
      }
    } catch (IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }
}
