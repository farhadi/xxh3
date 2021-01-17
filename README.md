# XXH3

[![CI build status](https://github.com/farhadi/xxh3/workflows/CI/badge.svg)](https://github.com/farhadi/xxh3/actions?query=workflow%3ACI)
[![Hex docs](http://img.shields.io/badge/hex.pm-docs-green.svg?style=flat)](https://hexdocs.pm/xxh3)
[![Hex Version](http://img.shields.io/hexpm/v/xxh3.svg?style=flat)](https://hex.pm/packages/xxh3)
[![License](http://img.shields.io/hexpm/l/xxh3.svg?style=flat)](https://github.com/farhadi/xxh3/blob/master/LICENSE)

Erlang NIF bindings for XXH3 hash functions implemented in Rust.

## Introduction

XXH3 is a new speed-optimized hash algorithm of the [xxHash](https://cyan4973.github.io/xxHash/)
family of non-cryptographic hash functions, featuring:

  - Improved speed for both small and large inputs
  - True 64-bit and 128-bit outputs
  - SIMD acceleration
  - Improved 32-bit viability

Speed analysis methodology is explained [here](https://fastcompression.blogspot.com/2019/03/presenting-xxh3.html).

## Implementation

Erlang nif interface does not support passing bigints to the VM, so for 128-bit variants
a 128-bit binary is passed, and then converted to a 128-bit integer in erlang.

## Usage
Note that only binary data is accepted. For other types of data you can
first convert them to a binary using `erlang:term_to_binary/1`.

In Erlang

```erlang
Data = <<"Test Data">>,
Seed = 1,
Secret = crypto:strong_rand_bytes(136),

H64 = xxh3:hash64(Data),
H64WithSeed = xxh3:hash64(Data, Seed),
H64WithSecret = xxh3:hash64_with_secret(Data, Secret),

H128 = xxh3:hash128(Data),
H128WithSeed = xxh3:hash128(Data, Seed),
H128WithSecret = xxh3:hash128_with_secret(Data, Secret).

H64Stream = xxh3:new(),
xxh3:update(H64Stream, Data),
H64 = xxh3:digest(H64Stream)
```

In Elixir

```elixir
data = "Test Data"
seed = 1
secret = :crypto.strong_rand_bytes(136)

h64 = :xxh3.hash64(data)
h64_with_seed = :xxh3.hash64(data, seed)
h64_with_secret = :xxh3.hash64_with_secret(data, secret)

h128 = :xxh3.hash128(data)
h128_with_seed = :xxh3.hash128(data, seed)
h128_with_secret = :xxh3.hash128_with_secret(data, secret)

h64_stream = :xxh3.new(),
:xxh3.update(h64_stream, data),
h64 = :xxh3.digest(h64_stream)
```

For more details, see the module documentation.

## License

Copyright 2021, Ali Farhadi <a.farhadi@gmail.com>.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
