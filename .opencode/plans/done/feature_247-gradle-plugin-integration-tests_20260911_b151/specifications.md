# Specifications

Create integration tests using Gradle TestKit to verify the plugin correctly generates OpenAPI and AsyncAPI code when tasks are executed via GradleRunner

## Functional Requirements

- Integration test applies plugin and runs openApiTask, verifying generated sources exist
- Integration test applies plugin and runs asyncApiTask, verifying generated sources exist
- Test project resources include minimal OpenAPI and AsyncAPI specs
- Tests run via GradleRunner with Gradle TestKit

## Non-Functional Requirements

- Tests must work offline (no external API calls)
- Tests use mavenLocal() for plugin resolution
- Tests follow existing test patterns in the project

## Acceptance Criteria

- gradleTestKit() dependency added to integrationTest suite
- At least one OpenAPI integration test passing
- At least one AsyncAPI integration test passing
- Tests run as part of check task

## Out of Scope

- Testing complex schema features (enums, combinators)
- Testing springwolf annotations
- Testing with real-world large specs
