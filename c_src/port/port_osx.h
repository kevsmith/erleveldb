// Copyright (c) 2011 The LevelDB Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file. See the AUTHORS file for names of contributors.
//
// See port_example.h for documentation for the following types/functions.

#ifndef STORAGE_LEVELDB_PORT_PORT_OSX_H_
#define STORAGE_LEVELDB_PORT_PORT_OSX_H_

#include <machine/endian.h>
#include <pthread.h>
#include <stdint.h>

#include <string>

#include "osx/atomicops.h"

extern "C" {
  size_t fread_unlocked(void *a, size_t b, size_t c, FILE *d);
  size_t fwrite_unlocked(const void *a, size_t b, size_t c, FILE *d);
  int fflush_unlocked(FILE *f);
  int fdatasync (int fd);
}

namespace leveldb {
namespace port {

static const bool kLittleEndian = (__DARWIN_BYTE_ORDER == __DARWIN_LITTLE_ENDIAN);

// ------------------ Threading -------------------

// A Mutex represents an exclusive lock.
class Mutex {
 public:
  Mutex();
  ~Mutex();

  void Lock();
  void Unlock();
  void AssertHeld() { }

 private:
  friend class CondVar;
  pthread_mutex_t mu_;

  // No copying
  Mutex(const Mutex&);
  void operator=(const Mutex&);
};

class CondVar {
 public:
  explicit CondVar(Mutex* mu);
  ~CondVar();

  void Wait();
  void Signal();
  void SignalAll();

 private:
  pthread_cond_t cv_;
  Mutex* mu_;
};

class AtomicPointer {
 private:
  typedef base::subtle::AtomicWord Rep;
  Rep rep_;
 public:
  AtomicPointer() { }
  explicit AtomicPointer(void* p) : rep_(reinterpret_cast<Rep>(p)) {}
  inline void* Acquire_Load() const {
    return reinterpret_cast<void*>(::base::subtle::Acquire_Load(&rep_));
  }
  inline void Release_Store(void* v) {
    ::base::subtle::Release_Store(&rep_, reinterpret_cast<Rep>(v));
  }
  inline void* NoBarrier_Load() const {
    return reinterpret_cast<void*>(::base::subtle::NoBarrier_Load(&rep_));
  }
  inline void NoBarrier_Store(void* v) {
    ::base::subtle::NoBarrier_Store(&rep_, reinterpret_cast<Rep>(v));
  }
};

inline bool Snappy_Compress(const char* input, size_t input_length,
                            std::string* output) {
    return false;
}

inline bool Snappy_Uncompress(const char* input_data, size_t input_length,
                              std::string* output) {
    return false;
}

inline bool GetHeapProfile(void (*func)(void*, const char*, int), void* arg) {
    return false;
}

}
}

#endif  // STORAGE_LEVELDB_PORT_PORT_OSX_H_
