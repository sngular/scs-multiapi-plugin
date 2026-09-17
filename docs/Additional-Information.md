# Additional Information

Comprehensive guides, references, and troubleshooting for the SCS MultiAPI Plugin.

## 📚 Documentation Index

### Guides

#### [OpenAPI Guide](OpenAPI-Guide)
Complete guide to OpenAPI 3.0+ features and best practices
- REST API generation
- Request/response handling
- Schema composition
- Advanced features

#### [AsyncAPI Guide](AsyncAPI-Guide)
Message-driven architecture with AsyncAPI
- Producer/consumer patterns
- Kafka integration
- RabbitMQ support
- Event modeling

#### [Spring-Kafka Integration](Spring-Kafka-Integration)
Deep dive into Spring Cloud Stream + Kafka integration
- AsyncAPI to Spring-Kafka workflow
- Supplier and StreamBridge patterns
- Error handling and retries
- Testing strategies

#### [Classpath Spec Loading](Classpath-Spec-Loading)
Load API specifications from classpath resources
- JAR-based spec loading
- Dependency resolution
- Multi-file specifications

#### [Dependency Spec Loading](Dependency-Spec-Loading)
Compose complex APIs using external dependency specs
- Spec composition
- Reference resolution
- Reusable components

### Configuration Reference

#### [Configuration Reference](Configuration-Reference)
All plugin configuration options for Maven and Gradle
- Plugin parameters
- Generation settings
- Output configuration
- Advanced options

### Architecture & Design

#### [Architecture Documentation](Architecture)
System design and architectural patterns
- Plugin architecture
- Code generation pipeline
- Schema processing
- Integration points

#### [Design Patterns](Design-Patterns)
Recommended patterns and best practices
- REST API patterns
- AsyncAPI patterns
- Validation patterns
- Spring Boot integration patterns

### Troubleshooting

#### [Troubleshooting Guide](Troubleshooting)
Solutions to common problems
- Build failures
- Generation issues
- IDE integration
- Runtime problems
- Performance tuning

### Implementation Details

#### [Implementation Notes](Implementation-Notes)
Technical implementation details and decisions
- Code generation algorithm
- Schema processing details
- Type mapping strategy
- Dependency resolution

## 🔍 Search by Use Case

### I want to build a REST API
1. Start: [Getting Started](Getting-Started)
2. Deepen: [OpenAPI Guide](OpenAPI-Guide)
3. Reference: [Configuration Reference](Configuration-Reference)
4. Troubleshoot: [Troubleshooting](Troubleshooting)

### I want to build a message-driven system
1. Start: [AsyncAPI Guide](AsyncAPI-Guide)
2. Deepen: [Spring-Kafka Integration](Spring-Kafka-Integration)
3. Reference: [Configuration Reference](Configuration-Reference)
4. Troubleshoot: [Troubleshooting](Troubleshooting)

### I want to compose multiple specs
1. Learn: [Dependency Spec Loading](Dependency-Spec-Loading)
2. Details: [Classpath Spec Loading](Classpath-Spec-Loading)
3. Example: Check examples in GitHub repository

### I need to configure advanced options
1. Reference: [Configuration Reference](Configuration-Reference)
2. Examples: Check GitHub repository
3. Patterns: [Design Patterns](Design-Patterns)

### I'm debugging a build issue
1. Check: [Troubleshooting](Troubleshooting)
2. Details: [Implementation Notes](Implementation-Notes)
3. Help: Open a GitHub issue

## 🛠️ Tools & Resources

### Maven
- [Maven Plugin Docs](https://mvnrepository.com/artifact/com.sngular/scs-multiapi-maven-plugin)
- Example: `mvn scs-multiapi:openapi-generation`
- Configuration: `pom.xml` under `<plugin>` section

### Gradle
- [Gradle Plugin Docs](https://plugins.gradle.org/plugin/com.sngular.scs-multiapi-gradle-plugin)
- Example: `gradle generateOpenapi`
- Configuration: `build.gradle` under plugin task section

### External Resources
- [OpenAPI 3.0 Specification](https://spec.openapis.org/oas/v3.0.3)
- [AsyncAPI 2.6 Specification](https://www.asyncapi.com/docs/specifications/v2.6.0)
- [Spring Cloud Stream](https://spring.io/projects/spring-cloud-stream)
- [Spring Boot Documentation](https://spring.io/projects/spring-boot)
- [Kafka Documentation](https://kafka.apache.org/documentation/)

## 📊 Feature Matrix

| Feature | OpenAPI | AsyncAPI | Maven | Gradle | Spring Boot 2.x | Spring Boot 3.x | Spring Boot 4.x |
|---------|---------|----------|-------|--------|-----------------|-----------------|-----------------|
| REST API Generation | ✅ | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Message Producers | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Message Consumers | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Kafka Binding | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| RabbitMQ Binding | ❌ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Validation | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Lombok Support | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Reactive Types | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Spec from Classpath | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Spec from HTTP | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Apicurio Registry | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Dependency Specs | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

## 📝 Examples & Samples

All examples are available in the [GitHub repository](https://github.com/sngular/scs-multiapi-plugin) under the `examples/` directory:

- `examples/openapi-rest-api` — Simple REST API
- `examples/asyncapi-kafka` — Kafka integration
- `examples/spring-boot-complete` — Full Spring Boot example
- `examples/multi-spec-composition` — Dependency specs example
- `examples/custom-validators` — Validation patterns

## ❓ FAQ

### Q: Can I use both OpenAPI and AsyncAPI in the same project?
A: Yes! Many projects use OpenAPI for REST endpoints and AsyncAPI for event streams.

### Q: What's the difference between Maven and Gradle plugins?
A: Functionally equivalent. Maven uses `pom.xml` configuration, Gradle uses `build.gradle`.

### Q: Can I customize the generated code?
A: Yes, see [Configuration Reference](Configuration-Reference) for customization options including package names, class names, and output paths.

### Q: How do I update the plugin to a newer version?
A: Update the version in your `pom.xml` or `build.gradle` and run `mvn clean generate-sources` or `gradle clean build`.

### Q: Can I use the plugin in CI/CD?
A: Yes. The plugin integrates with any Maven or Gradle CI/CD pipeline.

### Q: What's the performance impact of code generation?
A: Minimal. Generation typically takes 1-5 seconds for average-sized specs.

### Q: Can I exclude certain fields from generation?
A: Yes, use `x-` extensions in your spec or configure exclusions in plugin settings.

## 🐛 Reporting Issues

Found a bug or have a feature request? 
- **GitHub Issues**: [sngular/scs-multiapi-plugin/issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- **Include**: Plugin version, build system, Java version, full error message, and minimal reproduction spec

## 🤝 Contributing

Contributions are welcome! See [CONTRIBUTING.md](https://github.com/sngular/scs-multiapi-plugin/blob/main/CONTRIBUTING.md) for:
- Code style guidelines
- Pull request process
- Issue templates
- Development setup

## 📞 Support

- 📖 **Documentation**: Start here with [Getting Started](Getting-Started)
- 🔍 **Search**: Use the guide index above
- 💬 **Discussions**: [GitHub Discussions](https://github.com/sngular/scs-multiapi-plugin/discussions)
- 🐛 **Issues**: [GitHub Issues](https://github.com/sngular/scs-multiapi-plugin/issues)
- 📧 **Contact**: os3@sngular.com

## 📋 Version History

- **7.1.0** (Current) — Dependency spec loading, enhanced multipart support
- **7.0.x** — Full refactor, improved architecture
- **6.7.x** — Stable branch with bug fixes
- **6.0-6.6** — Legacy versions

See [CHANGELOG](CHANGELOG) for detailed version history.

---

**Back to**: [Home](Home) | [Getting Started](Getting-Started)

**Last Updated**: September 2026
