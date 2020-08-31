template <typename T>
class Source {
public:
  virtual ~Source() {}
  virtual bool fetch(T* dest) = 0;
};

template <typename T>
class ArraySource : public Source<T> {
public:
  ArraySource(T ts[], size_t n) {
    next_ = ts;
    end_ = ts + n;
  }
  bool fetch(T* dest) override {
    if (next_ >= end_) {
      return false;
    }
    *dest = *next_++;
    return true;
  }
private:
  T* next_;
  T* end_;
};

template<typename T>
class LocalPipeline {
public:
  explicit LocalPipeline(std::unique_ptr<Source<T>> src) : src_(std::move(src)) {}
  void run(std::function<void(T)> f) {
    T t;
    while (src_->fetch(&t)) {
      f(t);
    }
  }
private:
  std::unique_ptr<Source<T>> src_;
};
