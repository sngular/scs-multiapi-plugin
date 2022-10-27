/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

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
                                            .apiPackage("net.coru.scsplugin.filegeneration.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.filegeneration.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("subscribeOperationFileGeneration")
                                            .modelNameSuffix("Mapper")
                                            .apiPackage("net.coru.scsplugin.filegeneration.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.filegeneration.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_ISSUE_GENERATION = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueGeneration/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("response")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.issuegeneration.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.issuegeneration.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("clients")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.issuegeneration.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.issuegeneration.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_ISSUE = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationIssue/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("onCustomerEvent")
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.filegenerationissue.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.filegenerationissue.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("onCustomerOrderEvent")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.filegenerationissue.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.filegenerationissue.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_EXTERNAL_AVRO = List.of(
    SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationExternalAvro/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("subscribeOperationExternalAvro")
                                            .apiPackage("net.coru.scsplugin.externalavro.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.externalavro.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("publishOperationExternalAvro")
                                            .apiPackage("net.coru.scsplugin.externalavro.model.event.producer")
                                            .modelPackage("net.coru.scsplugin.externalavro.model.event")
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
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationWithoutOperationIds/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("net.coru.scsplugin.withoutids.model.event.consumer")
                                            .modelPackage("net.coru.scsplugin.withoutids.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .apiPackage("net.coru.scsplugin.withoutids.model.event.producer")
                                                .modelPackage("net.coru.scsplugin.withoutids.model.event")
                                                .modelNameSuffix("Mapper")
                                                .build())
          .build()
  );

  final static String TARGET = "target";

  final static String GENERATED = "generated/";

  static Function<Path, Boolean> validateTestFileGeneration() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/filegeneration/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/filegeneration/model/event/producer";

    String DEFAULT_MODEL_FOLDER = "generated/net/coru/scsplugin/filegeneration/model/event";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGeneration/assets/TestClassName.java");

    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGeneration/assets/Producer.java");

    List<String> expectedModelFiles = List.of(
      "asyncapigenerator/testFileGeneration/assets/CreateOrderMapper.java",
      "asyncapigenerator/testFileGeneration/assets/OrderCreatedDTO.java",
      "asyncapigenerator/testFileGeneration/assets/OrderDTO.java",
      "asyncapigenerator/testFileGeneration/assets/OrderLineDTO.java",
      "asyncapigenerator/testFileGeneration/assets/OrderLineMapper.java",
      "asyncapigenerator/testFileGeneration/assets/OrderMapper.java",
      "asyncapigenerator/testFileGeneration/assets/OrderProductDTO.java",
      "asyncapigenerator/testFileGeneration/assets/OrderProductMapper.java",
      "asyncapigenerator/testFileGeneration/assets/WaiterMapper.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
      modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGeneration() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/issuegeneration/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/issuegeneration/model/event/producer";

    String DEFAULT_MODEL_FOLDER = "generated/net/coru/scsplugin/issuegeneration/model/event";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testIssueGeneration/assets/IResponse.java",
        "asyncapigenerator/testIssueGeneration/assets/Subscriber.java");

    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testIssueGeneration/assets/IClients.java",
        "asyncapigenerator/testIssueGeneration/assets/Producer.java");

    List<String> expectedModelFiles = List.of(
      "asyncapigenerator/testIssueGeneration/assets/DataClientDTO.java",
      "asyncapigenerator/testIssueGeneration/assets/DataDTO.java",
      "asyncapigenerator/testIssueGeneration/assets/StatusDTO.java",
      "asyncapigenerator/testIssueGeneration/assets/StatusMsgDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
      modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationIssue() {

    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/filegenerationissue/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/filegenerationissue/model/event/producer";

    String DEFAULT_MODEL_FOLDER = "generated/net/coru/scsplugin/filegenerationissue/model/event";

    List<String> expectedConsumerFiles = List.of(
      "asyncapigenerator/testFileGenerationIssue/assets/IOnCustomerEvent.java",
      "asyncapigenerator/testFileGenerationIssue/assets/TestClassName.java");

    List<String> expectedProducerFiles = List.of(
      "asyncapigenerator/testFileGenerationIssue/assets/IOnCustomerOrderEvent.java",
      "asyncapigenerator/testFileGenerationIssue/assets/Producer.java");

    List<String> expectedModelFiles = List.of(
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerEventMessageDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerEventPayloadDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerOrderDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerOrderEventMessageDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/CustomerOrderEventPayloadDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/OrderedItemDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/PaymentDetailsDTO.java",
      "asyncapigenerator/testFileGenerationIssue/assets/ShippingDetailsDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationExternalAvro() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/externalavro/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/externalavro/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Subscriber.java");

    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Producer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationStreamBridge() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/streambridge/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/streambridge/model/event/producer";

    List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationStreamBridge/assets/TestClassName.java");
    List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/StreamBridgeProducer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithoutIds() {
    String DEFAULT_CONSUMER_FOLDER = "generated/net/coru/scsplugin/withoutids/model/event/consumer";

    String DEFAULT_PRODUCER_FOLDER = "generated/net/coru/scsplugin/withoutids/model/event/producer";

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

  private static boolean modelTest(final Path resultPath, final List<String> expectedModelFiles, final String default_model_folder) {
    Boolean result = Boolean.TRUE;
    try {
      Path pathToTarget = Path.of(resultPath.toString(), "target");

      if (!expectedModelFiles.isEmpty()) {
        Path pathToTargetProducer = pathToTarget.resolve(default_model_folder);
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
