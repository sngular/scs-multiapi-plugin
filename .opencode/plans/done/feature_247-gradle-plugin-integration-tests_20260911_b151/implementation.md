# Implementation Plan

Add GradleRunner integration tests using Gradle TestKit. Create test project with minimal specs, configure GradleRunner, assert generated output.

## Setup infrastructure

- [x] Add gradleTestKit() dep to integrationTest suite in build.gradle
- [x] Fix hardcoded version 6.7.4 to use project version
- [x] Create src/integrationTest directory structure

## Create test resources

- [x] Create minimal OpenAPI spec (src/integrationTest/resources/test-openapi/)
- [x] Create minimal AsyncAPI spec (src/integrationTest/resources/test-asyncapi/)
- [x] Create build.gradle for test projects

## Create test classes

- [x] Create OpenApiPluginIntegrationTest.java
- [x] Create AsyncApiPluginIntegrationTest.java

## Verify

- [x] Run integration tests
- [x] Fix any failures