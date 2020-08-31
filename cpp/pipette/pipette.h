#ifndef __PIPETTE_H__
#define __PIPETTE_H__

#include <functional>
#include <future>

#include "absl/memory/memory.h"

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
class Consumer {
public:
  virtual ~Consumer() {}
  virtual void consume(T t) = 0;
};

template<typename T>
class SinkNode : public Consumer<T> {
public:
  explicit SinkNode(std::function<void(T)> f) : f_(f) {}
  void consume(T t) override {
    std::async(std::bind(f_, t));
  }
private:
  std::function<void(T)> f_;
};

template<typename T>
class SourceNode {
public:
  explicit SourceNode(std::unique_ptr<Source<T>> src) : src_(std::move(src)) {}
  void set_sink(std::function<void(T)> f) {
    next_ = absl::make_unique<SinkNode<T>>(f);
  }
  bool fetch() {
    T t;
    if (src_->fetch(&t)) {
      next_->consume(t);
      return true;
    }
    return false;
  }
private:
  std::unique_ptr<Source<T>> src_;
  std::unique_ptr<Consumer<T>> next_;
};

template<typename T>
class LocalPipeline {
public:
  explicit LocalPipeline(std::unique_ptr<Source<T>> src) {
    src_ = absl::make_unique<SourceNode<T>>(std::move(src));
  }
  SourceNode<T>* source() {
    return src_.get();
  }
  void run() {
    while (src_->fetch()) {}
  }
private:
  std::unique_ptr<SourceNode<T>> src_;
};

#endif // __PIPETTE_H__
