//! `io` defines the basic interfaces for working
//! with a 6502 family i/o port.

/// Style defines the type of port being provided.
/// Either input, output or bidi style
#[derive(Clone, Copy)]
pub enum Style {
    /// `In` is an input only port.
    In(&'static dyn InputSink),
    /// `Out` is an output only port.
    Out(&'static dyn OutputSink),
}

/// `InputSink` is a receiver for the current state of an
/// input pin.
pub trait InputSink {
    /// Returns the current pin state (high or low)
    fn input(&self) -> bool;
}

/// `Pullup` represents a pullup resistor tied to an
/// input pin.
pub struct Pullup {}
impl InputSink for Pullup {
    fn input(&self) -> bool {
        true
    }
}

/// `PullDown` represents a grounded input pin
/// and is the default for `In` implementations.
pub struct PullDown {}
impl InputSink for PullDown {
    fn input(&self) -> bool {
        false
    }
}

/// `OutputSink` is a sender for the current state of an
/// input pin.
pub trait OutputSink {
    /// Returns the current pin state (high or low)
    fn output(&self) -> bool;
}

/// `OutputHigh` is an implementation of `OutputSink` showing a high signal
pub struct OutputHigh {}
impl OutputSink for OutputHigh {
    fn output(&self) -> bool {
        true
    }
}

/// `OutputLow` is an implementation of `OutputSink` showing a low signal
pub struct OutputLow {}
impl OutputSink for OutputLow {
    fn output(&self) -> bool {
        false
    }
}
