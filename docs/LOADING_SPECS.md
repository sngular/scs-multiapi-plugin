# Loading specs

Where the plugin looks for a contract, and which option to pick.

`filePath` is resolved in the order below; the first hit wins. Everything here
applies to both `openapi-generation` and `asyncapi-generation`, and to both build
tools.

Where the contract lives, and what to configure:

- **In this module** — `filePath` relative to the project directory.
- **In a published artifact** — `fromGroupId` and `fromArtifactId`, optionally
  `fromVersion`, with `filePath` naming the contract inside the artifact.
  `filePath` is optional when the artifact carries a single contract.
- **Behind a URL or in a registry** — `filePath` as the full `http(s)` URL.
- **In an artifact you add to the plugin itself** — `filePath` as the resource
  path. Single-file contracts only.

## From this module

The default. `filePath` is relative to the module's base directory:

```xml
<specFile>
  <filePath>src/main/resources/api.yml</filePath>
</specFile>
```

## From a published artifact

Use this when a team publishes its contracts as an artifact — the usual setup
when a producer and its consumers must not each keep their own copy.

### Maven

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.1.3</version>
  <executions>
    <execution>
      <goals><goal>openapi-generation</goal></goals>
      <configuration>
        <specFiles>
          <specFile>
            <filePath>contracts/api.yml</filePath>
            <fromGroupId>com.company</fromGroupId>
            <fromArtifactId>api-specs</fromArtifactId>
            <fromVersion>1.0.0</fromVersion>
            <apiPackage>com.example.api</apiPackage>
            <modelPackage>com.example.api.model</modelPackage>
          </specFile>
        </specFiles>
      </configuration>
    </execution>
  </executions>
</plugin>
```

### Gradle

```groovy
openapimodel {
  specFile {
    filePath = 'contracts/api.yml'
    fromGroupId = 'com.company'
    fromArtifactId = 'api-specs'
    fromVersion = '1.0.0'
    apiPackage = 'com.example.api'
    modelPackage = 'com.example.api.model'
  }
}
```

Points worth knowing:

- **`filePath` is the path inside the artifact**, not a path in your project. If
  you get it wrong the build fails listing the spec files the artifact does
  carry.
- **`filePath` can be omitted when the artifact carries exactly one contract**,
  which is the common case for a per-API artifact. A contract is a document with
  a top-level `openapi` or `asyncapi` field, so the schema fragments of a
  multi-file contract do not count, and an artifact publishing both an OpenAPI
  and an AsyncAPI contract still defaults correctly for each goal. With more than
  one contract of the same kind it is required again, and the build lists them
  rather than picking for you.
- **The artifact is fetched like any other dependency**, through the
  repositories your build is configured with. A contract published to a private
  repository works as long as that repository is declared and its credentials
  are in `settings.xml` (Maven) or the `repositories` block (Gradle). You do not
  have to declare the artifact as a project dependency.
- **`fromVersion` is optional.** Leave it out and the version already declared by
  the build is used, which keeps the artifact pinned in one place. With no
  declaration anywhere, the build fails asking for a version rather than
  guessing.
- **Multi-file contracts work.** The artifact is unpacked under
  `target/generated-resources/multiapi-specs` (Maven) or the equivalent Gradle
  build directory, so a `$ref` to another file inside the artifact resolves like
  any relative reference. Nothing is written to your sources.
- **Two artifacts can carry the same internal path**, because each spec names its
  own coordinates. That is the ambiguity this option exists to remove.

### Two contracts, two artifacts

A service that implements one API and calls another:

```xml
<specFiles>
  <specFile>
    <filePath>contracts/api.yml</filePath>
    <fromGroupId>com.company</fromGroupId>
    <fromArtifactId>api-warehouse</fromArtifactId>
    <apiPackage>com.example.infra.rest.api</apiPackage>
    <callMode>false</callMode>
  </specFile>
  <specFile>
    <filePath>contracts/api.yml</filePath>
    <fromGroupId>com.company</fromGroupId>
    <fromArtifactId>api-logistics</fromArtifactId>
    <apiPackage>com.example.infra.rest.client.logistics</apiPackage>
    <callMode>true</callMode>
  </specFile>
</specFiles>
```

Both specs are at `contracts/api.yml` inside their own artifact, and the
coordinates keep them apart.

### Packaging contracts as an artifact

An ordinary resources-only module:

```text
api-specs/
├── pom.xml
└── src/main/resources/
    └── contracts/
        ├── api.yml
        └── schemas/
            ├── user.yml
            └── common.yml
```

`mvn deploy` it, then reference `contracts/api.yml` with its coordinates.

## From a URL or a registry

Give `filePath` a full URL to read the contract from an HTTP endpoint or a
registry such as Apicurio. External `$ref`s are then resolved against the spec's
URL. Credentials come from system properties or environment variables — see
**Loading Specs from a Remote URL** in the [README](../README.md).

## From the plugin's own classpath

A contract inside an artifact that you add to the **plugin's** dependencies can
be named by its resource path:

```xml
<plugin>
  <groupId>com.sngular</groupId>
  <artifactId>scs-multiapi-maven-plugin</artifactId>
  <version>7.1.3</version>
  <dependencies>
    <dependency>
      <groupId>com.company</groupId>
      <artifactId>api-specs</artifactId>
      <version>1.0.0</version>
    </dependency>
  </dependencies>
  ...
  <specFile>
    <filePath>contracts/api.yml</filePath>
  </specFile>
</plugin>
```

Two limits make this the last resort:

- A **project** dependency is not on the plugin's classpath. Declaring the
  artifact in the project's `<dependencies>` and expecting this to work fails
  with `Error reading api file: <module>/<filePath>`; the artifact has to be
  inside the `<plugin>` block (or, in Gradle, on the buildscript classpath).
- Relative `$ref`s are resolved against the module directory, not against the
  artifact, so a **multi-file** contract fails on its first reference.

Prefer `fromGroupId`/`fromArtifactId`: it has neither limit.

## Troubleshooting

**`Error reading api file: <module>/<path> (No such file or directory)`** — the
contract was looked for on the filesystem. Either the path is wrong, or you meant
to load it from an artifact and the coordinates are missing: with only
`fromArtifactId` or only `fromGroupId` the build now stops and says so, but a
plain project dependency was never enough.

**`Spec '<path>' not found inside <coordinates>`** — the coordinates resolved but
the path inside the artifact does not exist. The message lists the `.yml`,
`.yaml` and `.json` files the artifact carries; pick the right one.

**`Cannot resolve <coordinates>`** — the artifact itself could not be fetched.
Check the coordinates and that the repository publishing it is declared in the
build and reachable, exactly as you would for any other dependency.

**`No version given for <groupId>:<artifactId>`** — `fromVersion` was omitted and
the artifact is not declared anywhere in the build. Set `fromVersion`, or declare
the artifact as a dependency of the module.

**`filePath is required for <coordinates>: the artifact carries N ... contracts`**
— the artifact publishes more than one contract, so there is nothing to default
to. The message lists them; name the one you want.
