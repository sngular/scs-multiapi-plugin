/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package com.sngular.api.generator.plugin.asyncapi;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Function;

import com.sngular.api.generator.plugin.asyncapi.parameter.OperationParameterObject;
import com.sngular.api.generator.plugin.asyncapi.parameter.SpecFile;
import com.sngular.api.generator.test.utils.TestUtils;

public class AsyncApiGeneratorFixtures {

  final static List<SpecFile> TEST_FILE_GENERATION = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGeneration/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("publishOperationFileGeneration")
                                            .classNamePostfix("TestClassName")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.filegeneration.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.filegeneration.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("subscribeOperationFileGeneration")
                                            .modelNameSuffix("Mapper")
                                            .apiPackage("com.sngular.scsplugin.filegeneration.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.filegeneration.model.event")
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
                                            .apiPackage("com.sngular.scsplugin.issuegeneration.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.issuegeneration.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("clients")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.issuegeneration.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.issuegeneration.model.event")
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
                                            .apiPackage("com.sngular.scsplugin.filegenerationissue.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.filegenerationissue.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("onCustomerOrderEvent")
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.filegenerationissue.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.filegenerationissue.model.event")
                                            .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_EXTERNAL_AVRO = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationExternalAvro/event-api.yml")
          .consumer(OperationParameterObject.builder()
                                            .ids("subscribeOperationExternalAvro")
                                            .apiPackage("com.sngular.scsplugin.externalavro.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.externalavro.model.event")
                                            .build())
          .supplier(OperationParameterObject.builder()
                                            .ids("publishOperationExternalAvro")
                                            .apiPackage("com.sngular.scsplugin.externalavro.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.externalavro.model.event")
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
                                            .apiPackage("com.sngular.scsplugin.streambridge.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.streambridge.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .ids("publishOperationStreamBridge")
                                                .apiPackage("com.sngular.scsplugin.streambridge.model.event.producer")
                                                .modelPackage("com.sngular.scsplugin.streambridge.model.event")
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
                                            .apiPackage("com.sngular.scsplugin.withoutids.model.event.consumer")
                                            .modelPackage("com.sngular.scsplugin.withoutids.model.event")
                                            .build())
          .streamBridge(OperationParameterObject.builder()
                                                .apiPackage("com.sngular.scsplugin.withoutids.model.event.producer")
                                                .modelPackage("com.sngular.scsplugin.withoutids.model.event")
                                                .modelNameSuffix("Mapper")
                                                .build())
          .build()
  );

  final static List<SpecFile> TEST_FILE_GENERATION_WITH_ARRAY_STRING = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testFileGenerationArrayString/event-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.arraywithstring.model.event.producer")
                                            .modelPackage("com.sngular.scsplugin.arraywithstring.model.event")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static List<SpecFile> TEST_ISSUE_GENERATE_SUPPLIER = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueGenerateSupplier/async-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("company.mail.messaging")
                                            .modelPackage("company.mail.model")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static List<SpecFile> TEST_ISSUE_INFINITE_LOOP = List.of(
      SpecFile
          .builder()
          .filePath("src/test/resources/asyncapigenerator/testIssueInfiniteLoop/async-api.yml")
          .supplier(OperationParameterObject.builder()
                                            .modelNameSuffix("DTO")
                                            .apiPackage("com.sngular.scsplugin.infiniteLoop.messaging")
                                            .modelPackage("com.sngular.scsplugin.infiniteLoop.model")
                                            .useLombokModelAnnotation(true)
                                            .build())
          .build());

  final static String TARGET = "target";

  final static String GENERATED = "generated/";

  static Function<Path, Boolean> validateTestFileGeneration() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event/producer";

    final String DEFAULT_MODEL_FOLDER = "generated/com/sngular/scsplugin/filegeneration/model/event";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGeneration/assets/TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGeneration/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGeneration/assets/Producer.java");

    final List<String> expectedModelFiles = List.of(
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
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event/producer";

    final String DEFAULT_MODEL_FOLDER = "generated/com/sngular/scsplugin/issuegeneration/model/event";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testIssueGeneration/assets/IResponse.java",
        "asyncapigenerator/testIssueGeneration/assets/Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testIssueGeneration/assets/IClients.java",
        "asyncapigenerator/testIssueGeneration/assets/Producer.java");

    final List<String> expectedModelFiles = List.of(
        "asyncapigenerator/testIssueGeneration/assets/DataClientDTO.java",
        "asyncapigenerator/testIssueGeneration/assets/DataDTO.java",
        "asyncapigenerator/testIssueGeneration/assets/StatusDTO.java",
        "asyncapigenerator/testIssueGeneration/assets/StatusMsgDTO.java"
    );

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER) &&
                     modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationIssue() {

    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event/producer";

    final String DEFAULT_MODEL_FOLDER = "generated/com/sngular/scsplugin/filegenerationissue/model/event";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationIssue/assets/IOnCustomerEvent.java",
        "asyncapigenerator/testFileGenerationIssue/assets/TestClassName.java");

    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationIssue/assets/IOnCustomerOrderEvent.java",
        "asyncapigenerator/testFileGenerationIssue/assets/Producer.java");

    final List<String> expectedModelFiles = List.of(
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
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/externalavro/model/event/producer";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Subscriber.java");

    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationExternalAvro/assets/IPublishOperation.java",
        "asyncapigenerator/testFileGenerationExternalAvro/assets/Producer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationStreamBridge() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/streambridge/model/event/producer";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationStreamBridge/assets/TestClassName.java");
    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationStreamBridge/assets/StreamBridgeProducer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationWithoutIds() {
    final String DEFAULT_CONSUMER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/consumer";

    final String DEFAULT_PRODUCER_FOLDER = "generated/com/sngular/scsplugin/withoutids/model/event/producer";

    final List<String> expectedConsumerFiles = List.of(
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/ISubscribeOperation.java",
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/TestClassName.java");
    final List<String> expectedProducerFiles = List.of(
        "asyncapigenerator/testFileGenerationWithoutOperationIds/assets/StreamBridgeProducer.java");

    return (path) -> commonTest(path, expectedConsumerFiles, expectedProducerFiles, DEFAULT_CONSUMER_FOLDER, DEFAULT_PRODUCER_FOLDER);
  }

  static Function<Path, Boolean> validateTestFileGenerationArrayString() {
    final String DEFAULT_MODEL_FOLDER = "generated/com/sngular/scsplugin/arraywithstring/model/event";

    final List<String> expectedModelFiles = List.of(
        "asyncapigenerator/testFileGenerationArrayString/assets/ObjectArrayDTO.java",
        "asyncapigenerator/testFileGenerationArrayString/assets/ObjectArrayMessageDTO.java");

    return path -> modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueGenerateSupplier() {
    final String DEFAULT_MODEL_FOLDER = "generated/company/mail/model";

    final List<String> expectedModelFiles = List.of(
        "asyncapigenerator/testIssueGenerateSupplier/assets/ConfigurationDTO.java",
        "asyncapigenerator/testIssueGenerateSupplier/assets/MailRequestDTO.java",
        "asyncapigenerator/testIssueGenerateSupplier/assets/MailRequestMessageDTO.java");

    return path -> modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  static Function<Path, Boolean> validateTestIssueInfiniteLoop() {
    final String DEFAULT_MODEL_FOLDER = "generated/com/sngular/scsplugin/infiniteLoop/model";

    final List<String> expectedModelFiles = List.of(
        //"asyncapigenerator/testIssueInfiniteLoop/assets/ConfigDTO.java",
        "asyncapigenerator/testIssueInfiniteLoop/assets/MailRequesInfiniteDTO.java");

    return path -> modelTest(path, expectedModelFiles, DEFAULT_MODEL_FOLDER);
  }

  private static Boolean commonTest(
      final Path resultPath, final List<String> expectedFile, final List<String> expectedModelFiles, final String targetConsumer, final String targetProducer) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");
      final Path pathToTargetConsumer = pathToTarget.resolve(targetConsumer);

      final File targetConsumerFolder = pathToTargetConsumer.toFile();
      assertThat(targetConsumerFolder).isNotEmptyDirectory();
      TestUtils.validateFiles(expectedFile, targetConsumerFolder);

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetProducer = pathToTarget.resolve(targetProducer);
        final File targetProducerFolder = pathToTargetProducer.toFile();
        assertThat(targetProducerFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetProducerFolder);
      }
    } catch (final IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }

  private static boolean modelTest(final Path resultPath, final List<String> expectedModelFiles, final String default_model_folder) {
    Boolean result = Boolean.TRUE;
    try {
      final Path pathToTarget = Path.of(resultPath.toString(), "target");

      if (!expectedModelFiles.isEmpty()) {
        final Path pathToTargetProducer = pathToTarget.resolve(default_model_folder);
        final File targetProducerFolder = pathToTargetProducer.toFile();
        assertThat(targetProducerFolder).isNotEmptyDirectory();
        TestUtils.validateFiles(expectedModelFiles, targetProducerFolder);
      }
    } catch (final IOException e) {
      result = Boolean.FALSE;
    }
    return result;
  }
}
