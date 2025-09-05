use super::interferences::Interferences;

/// Register Allocator
pub struct RegAlloc<const N_GPRS: usize> {
    interferences: Interferences,
}

impl<const N_GPRS: usize> RegAlloc<N_GPRS> { }
