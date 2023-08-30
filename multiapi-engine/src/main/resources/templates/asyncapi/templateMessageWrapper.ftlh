package ${wrapperPackage};

public class MessageWrapper<T, Y> {

  private T payload;

  private Y key;

  private MessageWrapper(final Y key, final T payload) {
    this.payload = payload;
    this.key = key;
  }

  public T getPayload() {
   return payload;
  }

  public void setPayload(T payload) {
    this.payload = payload;
  }

  public Y getKey() {
    return key;
  }

  public void setKey(Y key) {
    this.key = key;
  }

  public static MessageWrapperBuilder builder() {
    return new MessageWrapperBuilder();
  }

  public static class MessageWrapperBuilder<T, Y> {

    private T payload;

    private Y key;

    public MessageWrapperBuilder payload(final T payload) {
      this.payload = payload;
      return this;
    }

    public MessageWrapperBuilder key(final Y key) {
      this.key = key;
      return this;
    }

    public MessageWrapper build() {
      return new MessageWrapper(key, payload);
    }
  }
}