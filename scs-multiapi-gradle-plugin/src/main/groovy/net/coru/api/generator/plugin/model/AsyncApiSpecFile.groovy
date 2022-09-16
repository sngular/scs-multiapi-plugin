package net.coru.api.generator.plugin.model

import org.gradle.api.Action
import org.gradle.util.internal.ConfigureUtil

class AsyncApiSpecFile {

  String filePath

  OperationParameter supplier = new OperationParameter()

  OperationParameter consumer = new OperationParameter()

  OperationParameter streamBridge = new OperationParameter()

  void setFilePath(final String filePath) {
    this.filePath = filePath
  }

  String getFilePath() {
    return filePath
  }

  OperationParameter getSupplier() {
    return supplier
  }

  OperationParameter getConsumer() {
    return consumer
  }

  OperationParameter getStreamBridge() {
    return streamBridge
  }

  void supplier(Closure configuration) {
    ConfigureUtil.configure(configuration, supplier)
  }

  void supplier(Action<? super OperationParameter> configuration) {
    configuration.execute(supplier)
  }

  void consumer(Closure configuration) {
    ConfigureUtil.configure(configuration, consumer)
  }

  void consumer(Action<? super OperationParameter> configuration) {
    configuration.execute(consumer)
  }

  void streamBridge(Closure configuration) {
    ConfigureUtil.configure(configuration, streamBridge)
  }

  void streamBridge(Action<? super OperationParameter> configuration) {
    configuration.execute(streamBridge)
  }
}
