// Copyright (c) 2011 The LevelDB Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file. See the AUTHORS file for names of contributors.

#if defined(LEVELDB_PLATFORM_POSIX)
#  include "port/port_posix.cc"
#elif defined(LEVELDB_PLATFORM_OSX)
#  include "port/port_osx.cc"
#elif defined(LEVELDB_PLATFORM_CHROMIUM)
#  include "port/port_chromium.cc"
#elif defined(LEVELDB_PLATFORM_ANDROID)
#  include "port/port_android.cc"
#endif