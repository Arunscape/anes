#![warn(clippy::all)]

use std::collections::HashMap;
use std::sync::LazyLock;

#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPage_X,
    ZeroPage_Y,
    Absolute,
    Absolute_X,
    Absolute_Y,
    Indirect_X,
    Indirect_Y,
    Relative,
    NoneAddressing,
}

pub struct OpCode {
    pub code: u8,
    //    #[cfg(debug_assertions)]
    pub mnemonic: &'static str,
    pub len: u8,
    pub cycles: u8,
    pub addressing_mode: AddressingMode,
}

impl OpCode {
    fn new(
        code: u8,
        //#[cfg(debug_assertions)]
        mnemonic: &'static str,
        len: u8,
        cycles: u8,
        addressing_mode: AddressingMode,
    ) -> Self {
        Self {
            code,
            //           #[cfg(debug_assertions)]
            mnemonic,
            len,
            cycles,
            addressing_mode,
        }
    }
}

//#[cfg(debug_assertions)]
impl From<(u8, &'static str, u8, u8, AddressingMode)> for OpCode {
    fn from(
        (code, mnemonic, len, cycles, addressing_mode): (u8, &'static str, u8, u8, AddressingMode),
    ) -> Self {
        Self {
            code,
            mnemonic,
            len,
            cycles,
            addressing_mode,
        }
    }
}

//#[cfg(not(debug_assertions))]
//impl From<(u8, u8, u8, AddressingMode)> for OpCode {
//    fn from((code, len, cycles, addressing_mode): (u8, u8, u8, AddressingMode)) -> Self {
//        Self {
//            code,
//            len,
//            cycles,
//            addressing_mode,
//        }
//    }
//}

static OPCODES: LazyLock<Vec<OpCode>> = LazyLock::new(|| {
    [
        // (code  xxx, len,cycles, addressing_mode)
        //
        // ADC - Add with Carry
        (0x69, "ADC", 2, 2, AddressingMode::Immediate),
        (0x65, "ADC", 2, 2, AddressingMode::ZeroPage),
        (0x75, "ADC", 2, 2, AddressingMode::ZeroPage_X),
        (0x6D, "ADC", 2, 3, AddressingMode::Absolute),
        (0x7D, "ADC", 2, 3, AddressingMode::Absolute_X),
        (0x79, "ADC", 2, 3, AddressingMode::Absolute_Y),
        (0x61, "ADC", 2, 2, AddressingMode::Indirect_X),
        (0x71, "ADC", 2, 2, AddressingMode::Indirect_Y),
        //
        // AND - Logical AND
        (0x29, "AND", 2, 2, AddressingMode::Immediate),
        (0x25, "AND", 2, 3, AddressingMode::ZeroPage),
        (0x35, "AND", 2, 4, AddressingMode::ZeroPage_X),
        (0x2D, "AND", 3, 4, AddressingMode::Absolute),
        (
            0x3D,
            "AND",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0x39,
            "AND",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0x21, "AND", 2, 6, AddressingMode::Indirect_X),
        (
            0x31,
            "AND",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // ASL - Arithmetic Shift Left
        (0x0A, "ASL", 1, 2, AddressingMode::Immediate),
        (0x06, "ASL", 2, 5, AddressingMode::ZeroPage),
        (0x16, "ASL", 2, 6, AddressingMode::ZeroPage_X),
        (0x0E, "ASL", 3, 6, AddressingMode::Absolute),
        (0x1E, "ASL", 3, 7, AddressingMode::Absolute_X),
        //
        // BCC - Branch if Carry Clear
        (
            0x90,
            "BCC",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BCS - Branch if Carry Set
        (
            0xB0,
            "BCS",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BEQ - Branch if Equal
        (
            0xF0,
            "BEQ",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BIT - Bit Test
        (0x24, "BIT", 2, 3, AddressingMode::ZeroPage),
        (0x2C, "BIT", 3, 4, AddressingMode::Absolute),
        //
        // BMI - Branch if Minus
        (
            0x30,
            "BMI",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BNE - Branch if Not Equal
        (
            0xD0,
            "BNE",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BPL - Branch if Positive
        (
            0x10,
            "BPL",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BRK - Force Interrupt
        (0x00, "BRK", 1, 7, AddressingMode::Implied),
        //
        // BVC - Branch if Overflow Clear
        (
            0x50,
            "BVC",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // BVS - Brach if Overflow Set
        (
            0x70,
            "BVS",
            2,
            2, // +1 if branch succeeds, +2 if to a new page
            AddressingMode::Relative,
        ),
        //
        // CLC - Clear Carry Flag
        (0x18, "CLC", 1, 2, AddressingMode::Implied),
        //
        // CLD - Clear Decimal Mode
        (0xD8, "CLD", 1, 2, AddressingMode::Implied),
        //
        // CLI - Clear Interrupt Disable
        (0x58, "CLI", 1, 2, AddressingMode::Implied),
        //
        // CLV - Clear Overflow Flag
        (0xB8, "CLV", 1, 2, AddressingMode::Implied),
        //
        // CMP - Compare
        (0xC9, "CMP", 2, 2, AddressingMode::Immediate),
        (0xC5, "CMP", 2, 3, AddressingMode::ZeroPage),
        (0xD5, "CMP", 2, 4, AddressingMode::ZeroPage_X),
        (0xCD, "CMP", 3, 4, AddressingMode::Absolute),
        (
            0xDD,
            "CMP",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0xD9,
            "CMP",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0xC1, "CMP", 2, 6, AddressingMode::Indirect_X),
        (
            0xD1,
            "CMP",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // CPX - Compare X Register
        (0xE0, "CPX", 2, 2, AddressingMode::Immediate),
        (0xE4, "CPX", 2, 3, AddressingMode::ZeroPage),
        (0xEC, "CPX", 3, 4, AddressingMode::Absolute),
        //
        // CPY - Compare Y Register
        (0xC0, "CPY", 2, 2, AddressingMode::Immediate),
        (0xC4, "CPY", 2, 3, AddressingMode::ZeroPage),
        (0xCC, "CPY", 3, 4, AddressingMode::Absolute),
    ]
    .into_iter()
    .map(OpCode::from)
    .collect()
});

// https://bugzmanov.github.io/nes_ebook/chapter_3_1.html
// #[derive(Default)]
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub register_y: u8,
    pub status: u8,
    pub program_counter: u16,
    memory: [u8; 0xFFFF],
}

impl CPU {
    // I think I can replace with with #[derive(Default)]
    // nevermind can't anymore after implementing memory
    pub fn new() -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            register_y: 0,
            status: 0,
            program_counter: 0,
            memory: [0; 0xFFFF],
        }
    }

    fn get_operand_address(&mut self, mode: &AddressingMode) -> u16 {
        match mode {
            AddressingMode::Immediate => self.program_counter,

            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,

            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),

            AddressingMode::ZeroPage_X => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_x) as u16;
                addr
            }
            AddressingMode::ZeroPage_Y => {
                let pos = self.mem_read(self.program_counter);
                let addr = pos.wrapping_add(self.register_y) as u16;
                addr
            }

            AddressingMode::Absolute_X => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_x as u16);
                addr
            }
            AddressingMode::Absolute_Y => {
                let base = self.mem_read_u16(self.program_counter);
                let addr = base.wrapping_add(self.register_y as u16);
                addr
            }

            AddressingMode::Indirect_X => {
                let base = self.mem_read(self.program_counter);

                let ptr: u8 = (base as u8).wrapping_add(self.register_x);
                let lo = self.mem_read(ptr as u16);
                let hi = self.mem_read(ptr.wrapping_add(1) as u16);
                (hi as u16) << 8 | (lo as u16)
            }
            AddressingMode::Indirect_Y => {
                let base = self.mem_read(self.program_counter);

                let lo = self.mem_read(base as u16);
                let hi = self.mem_read((base as u8).wrapping_add(1) as u16);
                let deref_base = (hi as u16) << 8 | (lo as u16);
                let deref = deref_base.wrapping_add(self.register_y as u16);
                deref
            }

            // not sure about this one check it again TODO
            &AddressingMode::Relative => self.mem_read_u16(self.program_counter),

            AddressingMode::NoneAddressing => {
                panic!("{mode:?} mode is not supported");
            }
        }
    }

    // pub fn new() -> Self {
    //     Self::default()
    // }

    /// https://www.nesdev.org/obelisk-6502-guide/reference.html#LDA
    /// Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
    fn lda(&mut self, mode: &AddressingMode) {
        let addr = self.get_operand_address(mode);
        let value = self.mem_read(addr);

        self.register_a = value;
        self.update_zero_and_negative_flags(self.register_a);
    }

    /// https://www.nesdev.org/obelisk-6502-guide/reference.html#TAX
    /// Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate.
    fn tax(&mut self) {
        self.register_x = self.register_a;
        self.update_zero_and_negative_flags(self.register_x);
    }

    /// https://www.nesdev.org/obelisk-6502-guide/reference.html#INX
    /// Adds one to the X register setting the zero and negative flags as appropriate.
    fn inx(&mut self) {
        self.register_x = self.register_x.wrapping_add(1);
        self.update_zero_and_negative_flags(self.register_x);
    }

    fn update_zero_and_negative_flags(&mut self, result: u8) {
        if result == 0 {
            self.status |= 0b0000_0010;
        } else {
            self.status &= 0b1111_1101;
        }

        if result & 0b1000_0000 != 0 {
            self.status |= 0b1000_0000;
        } else {
            self.status &= 0b0111_1111;
        }
    }

    fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data;
    }

    fn mem_read_u16(&mut self, pos: u16) -> u16 {
        let lo = self.mem_read(pos) as u16;
        let hi = self.mem_read(pos + 1) as u16;
        (hi << 8) | (lo as u16)
    }

    fn mem_write_u16(&mut self, pos: u16, data: u16) {
        let hi = (data >> 8) as u8;
        let lo = (data & 0xff) as u8;
        self.mem_write(pos, lo);
        self.mem_write(pos + 1, hi);
    }

    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.run();
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.program_counter = 0x8000;
    }

    pub fn run(&mut self) {
        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;

            let opcode: OpCode = todo!();

            match code {
                0xA9 | 0xA5 | 0xB5 | 0xAD | 0xBD | 0xB9 | 0xA1 | 0xB1 => {
                    self.lda(&opcode.addressing_mode)
                }
                0xAA => self.tax(),
                0xE8 => self.inx(),
                //    0x69 | 0x65 | 0x75 | 0x6D | 0x7D | 0x79 | 0x61 | 0x71
                0x00 => return,
                _ => todo!(),
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;

    //    #[test]
    //    fn test_0xa9_lda_immediate_load_data() {
    //        let mut cpu = CPU::new();
    //        cpu.interpret(&[0xa9, 0x05, 0x00]);
    //        assert_eq!(cpu.register_a, 0x05);
    //        assert!(cpu.status & 0b0000_0010 == 0b00);
    //        assert!(cpu.status & 0b1000_0000 == 0);
    //    }
    //
    //    #[test]
    //    fn test_0xa9_lda_zero_flag() {
    //        let mut cpu = CPU::new();
    //        cpu.interpret(&[0xa9, 0x00, 0x00]);
    //        assert!(cpu.status & 0b0000_0010 == 0b10);
    //    }
    //
    //    #[test]
    //    fn test_0xa9_lda_negative_flag() {
    //        let mut cpu = CPU::new();
    //        cpu.interpret(&[0xa9, 0xff, 0x00]);
    //        assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    //    }
    //
    //    #[test]
    //    fn test_0xaa_tax_move_a_to_x() {
    //        let mut cpu = CPU::new();
    //        cpu.register_a = 10;
    //        cpu.interpret(&[0xaa, 0x00]);
    //
    //        assert_eq!(cpu.register_x, 10)
    //    }
    //    #[test]
    //    fn test_5_ops_working_together() {
    //        let mut cpu = CPU::new();
    //        cpu.interpret(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);
    //
    //        assert_eq!(cpu.register_x, 0xc1)
    //    }
    //
    //    #[test]
    //    fn test_inx_overflow() {
    //        let mut cpu = CPU::new();
    //        cpu.register_x = 0xff;
    //        cpu.interpret(&[0xe8, 0xe8, 0x00]);
    //
    //        assert_eq!(cpu.register_x, 1)
    //    }
}
