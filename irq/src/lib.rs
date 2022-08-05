//! `irq` defines the basic interfaces for working
//! with a 6502 family interrupt. A receiver of interrupts (IRQ/NMI)
//! will implement this trait to allow other components which generate
//! them to easily raise state without cross coupling component logic.
//! NOTE: Even though chips make a distinction between level and edge type interrupts
//!       the interfaces here don't matter and assume implementors simply account for
//!       this in clock cycle management.

/// Sender defines the trait for an IRQ source.
pub trait Sender {
    /// `raised` indicates whether the interrupt is currently held high.
    fn raised() -> bool;
}
