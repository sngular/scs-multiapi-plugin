/*
 *  This Source Code Form is subject to the terms of the Mozilla Public
 *  * License, v. 2.0. If a copy of the MPL was not distributed with this
 *  * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

package net.coru.api.generator.openapi.integration.test;

import static com.soebes.itf.extension.assertj.MavenITAssertions.assertThat;
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
public class OpenApiGenerationTest {

  private final String DEFAULT_TARGET_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi";

  private final String DEFAULT_MODEL_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model";

  private final String DEFAULT_EXCEPTION_API = "target/generated-sources/apigenerator/net/coru/multifileplugin/testapi/model/exception";

  @MavenTest
  @MavenGoal("${project.groupId}:${project.artifactId}:${project.version}:openapi-generation")
  void testApiMultiGeneration(MavenProjectResult result) throws IOException {
    List<File> expectedFileFirst = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                    "/TestFirstApi.java"));
    List<File> expectedFileSecond = List.of(new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets" +
                                                     "/TestSecondApi.java"));

    List<File> expectedExceptionFilesFirst = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionFirst.java"));

    List<File> expectedExceptionFilesSecond = List.of(
        new File("src/test/resources/net/coru/api/generator/openapi/integration/test/OpenApiGenerationTest/testApiMultiGeneration/assets/ModelClassExceptionSecond.java"));

    assertThat(result).hasTarget();
    Path pathToTarget = result.getTargetProjectDirectory().toPath();
    Path pathToTargetFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/first");
    Path pathToTargetSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/second");
    Path pathToExceptionFirst = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/first/model/exception");
    Path pathToExceptionSecond = pathToTarget.resolve("target/generated-sources/apigenerator/net/coru/multifileplugin/testmultifile/second/model/exception");

    File targetFirstFolder = pathToTargetFirst.toFile();
    assertThat(targetFirstFolder).isNotEmptyDirectory();

    File targetSecondFolder = pathToTargetSecond.toFile();
    assertThat(targetSecondFolder).isNotEmptyDirectory();

    File targetExceptionFirstFolder = pathToExceptionFirst.toFile();
    assertThat(targetExceptionFirstFolder).isNotEmptyDirectory();

    File targetExceptionSecondFolder = pathToExceptionSecond.toFile();
    assertThat(targetExceptionSecondFolder).isNotEmptyDirectory();

    validateFiles(expectedFileFirst, targetFirstFolder);
    validateFiles(expectedFileSecond, targetSecondFolder);
    validateFiles(expectedExceptionFilesFirst, targetExceptionFirstFolder);
    validateFiles(expectedExceptionFilesSecond, targetExceptionSecondFolder);
  }


}
