use rustler::types::atom::{self, Atom};
use rustler::types::binary::{Binary, OwnedBinary};
use rustler::{Error, Resource, ResourceArc};
use std::convert::TryInto;
use std::io::Write;
use std::sync::Mutex;
use xxhash_rust::xxh3::{self, Xxh3};

struct Xxh3Resource(Mutex<Xxh3>);

#[rustler::resource_impl]
impl Resource for Xxh3Resource {}

#[rustler::nif]
fn new() -> ResourceArc<Xxh3Resource> {
    ResourceArc::new(Xxh3Resource(Mutex::new(Xxh3::new())))
}

#[rustler::nif(name = "new")]
fn new_with_seed(seed: u64) -> ResourceArc<Xxh3Resource> {
    ResourceArc::new(Xxh3Resource(Mutex::new(Xxh3::with_seed(seed))))
}

#[rustler::nif]
fn new_with_secret(secret: Binary) -> Result<ResourceArc<Xxh3Resource>, Error> {
    Ok(ResourceArc::new(Xxh3Resource(Mutex::new(
        Xxh3::with_secret(secret.as_slice().try_into().map_err(|_| Error::BadArg)?),
    ))))
}

#[rustler::nif]
fn update(resource: ResourceArc<Xxh3Resource>, data: Binary) -> Atom {
    resource.0.lock().unwrap().update(data.as_slice());
    atom::ok()
}

#[rustler::nif]
fn reset(resource: ResourceArc<Xxh3Resource>) -> Atom {
    resource.0.lock().unwrap().reset();
    atom::ok()
}

#[rustler::nif]
fn digest(resource: ResourceArc<Xxh3Resource>) -> u64 {
    resource.0.lock().unwrap().digest()
}

#[rustler::nif]
fn hash64(data: Binary) -> u64 {
    xxh3::xxh3_64(data.as_slice())
}

#[rustler::nif(name = "hash64")]
fn hash64_with_seed(data: Binary, seed: u64) -> u64 {
    xxh3::xxh3_64_with_seed(data.as_slice(), seed)
}

#[rustler::nif]
fn hash64_with_secret(data: Binary, secret: Binary) -> u64 {
    xxh3::xxh3_64_with_secret(data.as_slice(), secret.as_slice())
}

#[rustler::nif]
fn hash128_with_seed_bin(data: Binary, seed: u64) -> OwnedBinary {
    let hash = xxh3::xxh3_128_with_seed(data.as_slice(), seed);
    let mut binary = OwnedBinary::new(16).unwrap();
    let _ = binary.as_mut_slice().write_all(&hash.to_be_bytes());
    binary
}

#[rustler::nif]
fn hash128_with_secret_bin(data: Binary, secret: Binary) -> OwnedBinary {
    let hash = xxh3::xxh3_128_with_secret(data.as_slice(), secret.as_slice());
    let mut binary = OwnedBinary::new(16).unwrap();
    let _ = binary.as_mut_slice().write_all(&hash.to_be_bytes());
    binary
}

rustler::init!("xxh3");
