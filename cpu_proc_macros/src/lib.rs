//! Proc macros for adding attribute `CPU65xx` to build up the basic CPU struct
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{self, Parser},
    parse_macro_input, ItemStruct,
};

#[proc_macro_attribute]
/// Define the mapping for the given struct with basic CPU members all 65xx
/// require.
///
/// # Panics
/// This only works on a struct type.
#[allow(clippy::unwrap_used, clippy::too_many_lines)]
pub fn cpu_base_struct(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut item_struct = parse_macro_input!(input as ItemStruct);
    let _ = parse_macro_input!(args as parse::Nothing);

    if let syn::Fields::Named(ref mut fields) = item_struct.fields {
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Accumulator register"]
                  a: Wrapping<u8>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " X register"]
                  x: Wrapping<u8>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Y register"]
                  y: Wrapping<u8>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Stack pointer"]
                  s: Wrapping<u8>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Status register"]
                  p: Flags
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Program counter"]
                  pc: Wrapping<u16>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " If set `debug` will be passed a raw `CPUState` on each instruction."]
                  #[doc = " The boolean returns indicates whether to include a full memory dump (slow)"]
                  #[doc = " or just to fill in the current PC values so dissembly can function."]
                  debug: Option<&'a dyn Fn() -> (Rc<RefCell<CPUState>>, bool)>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Initialized or not."]
                  state: State
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " State of interrupt lines."]
                  irq_raised: InterruptStyle
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Optional IRQ line"]
                  irq: Option<&'a dyn irq::Sender>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Optional NMI line"]
                  nmi: Option<&'a dyn irq::Sender>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Optional RDY line"]
                  rdy: Option<&'a dyn irq::Sender>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Whether we're currently running an interrupt"]
                  interrupt_state: InterruptState
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " If set we're skipping starting an interrupt for one clock cycle."]
                  skip_interrupt: SkipInterrupt
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Tracking for reset when we need to clear the extra clocks"]
                  #[doc = " up front before simulating BRK. If `tick` is called and this"]
                  #[doc = " isn't in ResetTick::Reset an error will result."]
                  reset_tick: ResetTick
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = "Total number of clock cycles since start."]
                  clocks: usize
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Memory implementation used for all RAM access."]
                  ram: Rc<RefCell<RecordRAM>>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " The current working opcode."]
                  op: Operation
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " The raw opcode value."]
                  op_raw: u8
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = "  The 1st byte argument after the opcode (all instruction have this)."]
                  #[doc = "  Often used as a temp value while building the whole instruction."]
                  op_val: u8
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Tick number for internal operation of opcode."]
                  op_tick: Tick
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Address computed during opcode to be used for read/write (indirect, etc modes)."]
                  op_addr: u16
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Stays OpState::Processing until the current opcode has completed any addressing mode ticks."]
                  #[doc = " NOTE: This is instruction dependent as to whether it gets updated. i.e. NOP may just run through"]
                  #[doc = "       addressing mode cycles and complete without bothering to set this since nothing else in that"]
                  #[doc = "       instruction will care. Constrast to a RMW instruction which has to run a cycle one past when"]
                  #[doc = "       this is marked Done."]
                  addr_done: OpState
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " The opcode value used to halt the CPU."]
                  halt_opcode: u8
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " The PC value of the halt instruction."]
                  halt_pc: u16
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Preallocated dissassembly buffer for debugging."]
                  disassemble: RefCell<String>
                })
                .unwrap(),
        );
        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! {
                  #[doc = " Whether the current address mode had to use an extra cycle to fix op_addr."]
                  op_addr_fixup: bool
                })
                .unwrap(),
        );
    }

    quote! {
        #item_struct
    }
    .into()
}
