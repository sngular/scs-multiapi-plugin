/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.asyncapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
import static net.coru.api.generator.test.utils.TestUtils.checkTargetFiles;
import static net.coru.api.generator.test.utils.TestUtils.validateFiles;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

import com.soebes.itf.jupiter.extension.MavenGoal;
import com.soebes.itf.jupiter.extension.MavenJupiterExtension;
import com.soebes.itf.jupiter.extension.MavenRepository;
import com.soebes.itf.jupiter.extension.MavenTest;
import com.soebes.itf.jupiter.maven.MavenProjectResult;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

@MavenRepository
@MavenJupiterExtension
@Execution(ExecutionMode.SAME_THREAD)
public class AsyncApiGenerationTest {

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:asyncapi-generation")
  void testFileGenerationTwoYmlFiles(MavenProjectResult result) throws IOException {
    List<String> expectedFirstYmlFileNames = List.of("IPublishOperation.java", "ISubscribeOperation.java", "Producer.java", "Subscriber.java");

    List<String> expectedSecondYmlFileNames = List.of("Producer.java", "IPublishOperation2.java");

    List<String> expectedFirstYmlFiles = List.of(
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/IPublishOperation.java",
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/ISubscribeOperation.java",
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Producer.java",
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/Subscriber.java");

    List<String> expectedSecondYmlFiles = List.of(
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/IPublishOperation2.java",
        "net/coru/api/generator/asyncapi/integration/test/AsyncApiGenerationTest/testFileGenerationTwoYmlFiles/assets/producer2/Producer.java");

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();

    Path pathToTargetFirstYml = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru");
    Path pathToTargetSecondYml = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/generator/multiapi/model/event/producer2");

    File targetFirstYmlDirectory = pathToTargetFirstYml.toFile();
    File targetSecondYmlDirectory = pathToTargetSecondYml.toFile();

    checkTargetFiles(expectedFirstYmlFileNames, targetFirstYmlDirectory);
    checkTargetFiles(expectedSecondYmlFileNames, targetSecondYmlDirectory);
    validateFiles(expectedFirstYmlFiles, targetFirstYmlDirectory);
    validateFiles(expectedSecondYmlFiles, targetSecondYmlDirectory);

  }


}
