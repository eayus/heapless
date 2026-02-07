#![allow(unused_parens)]
#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unconditional_recursion)]
#![no_std]
#![no_main]

use core::panic::PanicInfo;

#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[link(name = "runtime")]
extern "C" {
    fn read_int_c() -> usize;
    fn print_int_c(n: usize);
}

fn read_int(_world: ()) -> ((), usize) {
    unsafe { ((), read_int_c()) }
}

fn print_int(n: usize, _world: ()) -> () {
    unsafe { print_int_c(n) }
}
