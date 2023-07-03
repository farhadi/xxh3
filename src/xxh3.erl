%%%-------------------------------------------------------------------
%% @doc NIF bindings for XXH3 hash functions implemented in Rust
%%
%% XXH3 is a new speed-optimized hash algorithm of the xxHash family
%% of non-cryptographic hash functions, featuring:
%% <ul>
%% <li>Improved speed for both small and large inputs</li>
%% <li>True 64-bit and 128-bit outputs</li>
%% <li>SIMD acceleration</li>
%% <li>Improved 32-bit viability</li>
%% </ul>
%%
%% Speed analysis methodology is explained here:
%%
%%    https://fastcompression.blogspot.com/2019/03/presenting-xxh3.html
%%
%% @end
%%%-------------------------------------------------------------------

-module(xxh3).

-export([
    new/0, new/1,
    new_with_secret/1,
    update/2,
    reset/1,
    digest/1,
    hash64/1, hash64/2,
    hash64_with_secret/2,
    hash128/1, hash128/2,
    hash128_with_secret/2
]).

-include("cargo.hrl").

-opaque xxh3_ref() :: reference().

-export_type([xxh3_ref/0]).

-define(not_loaded, not_loaded(?LINE)).

-on_load(init/0).

init() ->
    ?load_nif_from_crate(xxh3, 0).

%% @doc Creates a new 64-bit hasher with default secret.
%%
%% You can stream data to the returned object using {@link update/2},
%% and calculate intermediate hash values using {@link digest/1}.
-spec new() -> xxh3_ref().
new() ->
    ?not_loaded.

%% @doc Creates a new 64-bit hasher with the given `Seed'.
-spec new(non_neg_integer()) -> xxh3_ref().
new(_Seed) ->
    ?not_loaded.

%% @doc Creates a new 64-bit hasher with the given `Secret'.
%%
%% `Secret' must be a binary of size 192 bytes.
-spec new_with_secret(binary()) -> xxh3_ref().
new_with_secret(_Secret) ->
    ?not_loaded.

%% @doc Updates hasher state with the given chunk of data.
-spec update(xxh3_ref(), binary()) -> ok.
update(_Resource, _Data) ->
    ?not_loaded.

%% @doc Resets hasher state.
-spec reset(xxh3_ref()) -> ok.
reset(_Resource) ->
    ?not_loaded.

%% @doc Computes hash for streamed data.
-spec digest(xxh3_ref()) -> non_neg_integer().
digest(_Resource) ->
    ?not_loaded.

%% @doc Returns 64-bit hash for the given `Data'.
%%
%% This is default 64-bit variant, using default secret and default seed of 0.
%% It's the fastest variant.
-spec hash64(binary()) -> non_neg_integer().
hash64(_Data) ->
    ?not_loaded.

%% @doc Returns 64-bit hash for the given `Data' with `Seed' value.
%%
%% This variant generates a custom secret on the fly
%% based on default secret altered using the `Seed' value.
%% While this operation is decently fast, note that it's not completely free.
-spec hash64(binary(), non_neg_integer()) -> non_neg_integer().
hash64(_Data, _Seed) ->
    ?not_loaded.

%% @doc Returns 64-bit hash for the given `Data' with a custom `Secret'.
%%
%% It's possible to provide any binary as a "secret" to generate the hash.
%% This makes it more difficult for an external actor to prepare an intentional collision.
%% The main condition is that `Secret' size *must* be large enough (>= 136 bytes).
%% However, the quality of produced hash values depends on secret's entropy.
%% Technically, the secret must look like a bunch of random bytes.
%% Avoid "trivial" or structured data such as repeated sequences or a text document.
-spec hash64_with_secret(binary(), binary()) -> non_neg_integer().
hash64_with_secret(_Data, _Secret) ->
    ?not_loaded.

%% @doc Returns 128-bit hash for the given `Data'.
%%
%% This is default 128-bit variant, using default secret and default seed of 0.
-spec hash128(binary()) -> non_neg_integer().
hash128(Data) ->
    binary:decode_unsigned(hash128_with_seed_bin(Data, 0)).

%% @doc Returns 128-bit hash for the given `Data' with `Seed' value.
%%
%% See {@link hash64/2} for more details.
-spec hash128(binary(), non_neg_integer()) -> non_neg_integer().
hash128(Data, Seed) ->
    binary:decode_unsigned(hash128_with_seed_bin(Data, Seed)).

%% @doc Returns 128-bit hash for the given `Data' with a custom `Secret'.
%%
%% See {@link hash64_with_secret/2} for more details.
-spec hash128_with_secret(binary(), binary()) -> non_neg_integer().
hash128_with_secret(Data, Secret) ->
    binary:decode_unsigned(hash128_with_secret_bin(Data, Secret)).

%%%-------------------------------------------------------------------
%% Internal functions
%%%-------------------------------------------------------------------

hash128_with_seed_bin(_Data, _Seed) ->
    ?not_loaded.

hash128_with_secret_bin(_Data, _Secret) ->
    ?not_loaded.

not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
