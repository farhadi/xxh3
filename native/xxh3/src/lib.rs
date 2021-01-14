use rustler::types::binary::{Binary, OwnedBinary};
use std::io::Write;
use twox_hash::xxh3;

#[rustler::nif]
fn hash64_with_seed(data: Binary, seed: u64) -> u64 {
    xxh3::hash64_with_seed(data.as_slice(), seed)
}

#[rustler::nif]
fn hash64_with_secret(data: Binary, secret: Binary) -> u64 {
    xxh3::hash64_with_secret(data.as_slice(), secret.as_slice())
}

#[rustler::nif]
fn hash128_with_seed_bin(data: Binary, seed: u64) -> OwnedBinary {
    let hash = xxh3::hash128_with_seed(data.as_slice(), seed);
    let mut binary = OwnedBinary::new(16).unwrap();
    let _ = binary.as_mut_slice().write_all(&hash.to_be_bytes());
    binary
}

#[rustler::nif]
fn hash128_with_secret_bin(data: Binary, secret: Binary) -> OwnedBinary {
    let hash = xxh3::hash128_with_secret(data.as_slice(), secret.as_slice());
    let mut binary = OwnedBinary::new(16).unwrap();
    let _ = binary.as_mut_slice().write_all(&hash.to_be_bytes());
    binary
}

rustler::init!(
    "xxh3",
    [
        hash64_with_seed,
        hash64_with_secret,
        hash128_with_seed_bin,
        hash128_with_secret_bin
    ]
);
