#![warn(clippy::all)]

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
    Implied,
    Accumulator,
    Indirect,
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
        //
        // DEC - Decrement Memory
        (0xC6, "DEC", 2, 5, AddressingMode::ZeroPage),
        (0xD6, "DEC", 2, 6, AddressingMode::ZeroPage_X),
        (0xCE, "DEC", 3, 6, AddressingMode::Absolute),
        (0xDE, "DEC", 3, 7, AddressingMode::Absolute_X),
        //
        // DEX - Decrement X Register
        (0xCA, "DEX", 1, 2, AddressingMode::Implied),
        //
        // DEY - Decrement X Register
        (0x88, "DEY", 1, 2, AddressingMode::Implied),
        //
        // EOR - Exclusive OR
        (0x49, "EOR", 2, 2, AddressingMode::Immediate),
        (0x45, "EOR", 2, 3, AddressingMode::ZeroPage),
        (0x55, "EOR", 2, 4, AddressingMode::ZeroPage_X),
        (0x4D, "EOR", 3, 4, AddressingMode::Absolute),
        (
            0x5D,
            "EOR",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0x59,
            "EOR",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0x41, "EOR", 2, 6, AddressingMode::Indirect_X),
        (
            0x51,
            "EOR",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // INC - Increment Memory
        (0xE6, "INC", 2, 5, AddressingMode::ZeroPage),
        (0xF6, "INC", 2, 6, AddressingMode::ZeroPage_X),
        (0xEE, "INC", 3, 6, AddressingMode::Absolute),
        (0xFE, "INC", 3, 7, AddressingMode::Absolute_X),
        //
        // INX - Increment X Register
        (0xE8, "INX", 1, 2, AddressingMode::Implied),
        //
        // INY - Increment Y Register
        (0xC8, "INY", 1, 2, AddressingMode::Implied),
        //
        // JMP - Jump
        (0x4C, "JMP", 3, 3, AddressingMode::Absolute),
        (0x6C, "JMP", 3, 5, AddressingMode::Indirect),
        //
        // JSR - Jump to Subroutine
        (0x20, "JSR", 3, 6, AddressingMode::Absolute),
        //
        // LDA - Load accumulator
        (0xA9, "LDA", 2, 2, AddressingMode::Immediate),
        (0xA5, "LDA", 2, 3, AddressingMode::ZeroPage),
        (0xB5, "LDA", 2, 4, AddressingMode::ZeroPage_X),
        (0xAD, "LDA", 3, 4, AddressingMode::Absolute),
        (
            0xBD,
            "LDA",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0xB9,
            "LDA",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0xA1, "LDA", 2, 6, AddressingMode::Indirect_X),
        (
            0xB1,
            "LDA",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // LDX - Load X Register
        (0xA2, "LDX", 2, 2, AddressingMode::Immediate),
        (0xA6, "LDX", 2, 3, AddressingMode::ZeroPage),
        (0xB6, "LDX", 2, 4, AddressingMode::ZeroPage_Y),
        (0xAE, "LDX", 3, 4, AddressingMode::Absolute),
        (
            0xBE,
            "LDX",
            3,
            4, //+1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        //
        // LDY - Load Y Register
        (0xA0, "LDY", 2, 2, AddressingMode::Immediate),
        (0xA4, "LDY", 2, 3, AddressingMode::ZeroPage),
        (0xB4, "LDY", 2, 4, AddressingMode::ZeroPage_X),
        (0xAC, "LDY", 3, 4, AddressingMode::Absolute),
        (
            0xBC,
            "LDY",
            3,
            4, //+1 if page crossed
            AddressingMode::Absolute_X,
        ),
        //
        // LSR - Logical Shift Right
        (0x4A, "LSR", 1, 2, AddressingMode::Accumulator),
        (0x46, "LSR", 2, 5, AddressingMode::ZeroPage),
        (0x56, "LSR", 2, 6, AddressingMode::ZeroPage_X),
        (0x4E, "LSR", 3, 6, AddressingMode::Absolute),
        (0x5E, "LSR", 3, 7, AddressingMode::Absolute_X),
        //
        // NOP - No Operation
        (0xEA, "NOP", 1, 2, AddressingMode::Implied),
        //
        // ORA - Logical Inclusive OR
        (0x09, "ORA", 2, 2, AddressingMode::Immediate),
        (0x05, "ORA", 2, 3, AddressingMode::ZeroPage),
        (0x15, "ORA", 2, 4, AddressingMode::ZeroPage_X),
        (0x0D, "ORA", 3, 4, AddressingMode::Absolute),
        (
            0x1D,
            "ORA",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0x19,
            "ORA",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0x01, "ORA", 2, 6, AddressingMode::Indirect_X),
        (
            0x11,
            "ORA",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // PHA - Push Accumulator
        (0x48, "PHA", 1, 3, AddressingMode::Implied),
        //
        // PHP - Push Processor Status
        (0x08, "PHP", 1, 3, AddressingMode::Implied),
        //
        // PLA - Pull Accumulator
        (0x68, "PLA", 1, 4, AddressingMode::Implied),
        //
        // PLP - Pull Processor Status
        (0x28, "PLP", 1, 4, AddressingMode::Implied),
        //
        // ROL - Rotate Left
        (0x2A, "ROL", 1, 2, AddressingMode::Accumulator),
        (0x26, "ROL", 2, 5, AddressingMode::ZeroPage),
        (0x36, "ROL", 2, 6, AddressingMode::ZeroPage_X),
        (0x2E, "ROL", 3, 6, AddressingMode::Absolute),
        (0x3E, "ROL", 3, 7, AddressingMode::Absolute_X),
        //
        // ROR - Rotate Right
        (0x2A, "ROR", 1, 2, AddressingMode::Accumulator),
        (0x26, "ROR", 2, 5, AddressingMode::ZeroPage),
        (0x36, "ROR", 2, 6, AddressingMode::ZeroPage_X),
        (0x2E, "ROR", 3, 6, AddressingMode::Absolute),
        (0x3E, "ROL", 3, 7, AddressingMode::Absolute_X),
        //
        // RTI - Return from Interrupt
        (0x40, "RTI", 1, 6, AddressingMode::Implied),
        //
        // RTS - Return from Subroutine
        (0x60, "RTS", 1, 6, AddressingMode::Implied),
        //
        // SBC - Subtract with Carry
        (0xE9, "SBC", 2, 2, AddressingMode::Immediate),
        (0xE5, "SBC", 2, 3, AddressingMode::ZeroPage),
        (0xF5, "SBC", 2, 4, AddressingMode::ZeroPage_X),
        (0xED, "SBC", 3, 4, AddressingMode::Absolute),
        (
            0xFD,
            "SBC",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_X,
        ),
        (
            0xF9,
            "SBC",
            3,
            4, // +1 if page crossed
            AddressingMode::Absolute_Y,
        ),
        (0xE1, "SBC", 2, 6, AddressingMode::Indirect_X),
        (
            0xF1,
            "SBC",
            2,
            5, // +1 if page crossed
            AddressingMode::Indirect_Y,
        ),
        //
        // SEC - Set Carry Flag
        (0x38, "SEC", 1, 2, AddressingMode::Implied),
        //
        // SED - Set Decimal Flag
        (0xF8, "SED", 1, 2, AddressingMode::Implied),
        //
        // SEI - Set Inturrupt Disable
        (0x78, "SEI", 1, 2, AddressingMode::Implied),
        //
        // STA - Store Accumulator
        (0x85, "STA", 2, 3, AddressingMode::ZeroPage),
        (0x95, "STA", 2, 4, AddressingMode::ZeroPage_X),
        (0x8D, "STA", 3, 4, AddressingMode::Absolute),
        (0x9D, "STA", 3, 5, AddressingMode::Absolute_X),
        (0x99, "STA", 3, 5, AddressingMode::Absolute_Y),
        (0x81, "STA", 2, 6, AddressingMode::Indirect_X),
        (0x91, "STA", 2, 6, AddressingMode::Indirect_Y),
        //
        // STX - Store X Register
        (0x86, "STX", 2, 3, AddressingMode::ZeroPage),
        (0x96, "STX", 2, 4, AddressingMode::ZeroPage_Y),
        (0x8E, "STX", 3, 4, AddressingMode::Absolute),
        //
        // STY - Store Y Register
        (0x84, "STY", 2, 3, AddressingMode::ZeroPage),
        (0x94, "STY", 2, 4, AddressingMode::ZeroPage_X),
        (0x8C, "STY", 3, 4, AddressingMode::Absolute),
        //
        // TAX - Transfer Accumulator to X
        (0xAA, "TAX", 1, 2, AddressingMode::Implied),
        //
        // TAY - Transfer Accumulator to Y
        (0xA8, "TAY", 1, 2, AddressingMode::Implied),
        //
        // TSX - Transfer Stack Pointer to X
        (0xBA, "TSX", 1, 2, AddressingMode::Implied),
        //
        // TXA - Transfer X to Accumulator
        (0x8A, "TXA", 1, 2, AddressingMode::Implied),
        //
        // TXS - Transfer X to Stack Pointer
        (0x9A, "TXS", 1, 2, AddressingMode::Implied),
        //
        // TYA - Transfer Y to Accumulator
        (0x98, "TYA", 1, 2, AddressingMode::Implied),
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

            AddressingMode::NoneAddressing
            | AddressingMode::Implied
            | AddressingMode::Accumulator
            | AddressingMode::Indirect => {
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

    //    pub fn interpret(&mut self, program: &[u8]) {
    //        self.program_counter = 0;
    //
    //        loop {
    //            let opscode = program[self.program_counter as usize];
    //            self.program_counter += 1;
    //
    //            match opscode {
    //                0xA9 => {
    //                    let param = program[self.program_counter as usize];
    //                    self.program_counter += 1;
    //
    //                    self.lda(&AddressingMode::Immediate);
    //                }
    //
    //                0xAA => self.tax(),
    //
    //                0xe8 => self.inx(),
    //
    //                0x00 => return,
    //
    //                _ => todo!(),
    //            }
    //        }
    //    }

    pub fn load_and_run(&mut self, program: &[u8]) {
        self.load(program);
        self.run();
    }

    pub fn load(&mut self, program: &[u8]) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.program_counter = 0x8000;
    }

    pub fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        loop {
            let code = self.mem_read(self.program_counter);
            self.program_counter += 1;
            let program_counter_state = self.program_counter;

            let opcode = OPCODES.get(code as usize).ok_or("")?;

            match code {
                0xa9 | 0xa5 | 0xb5 | 0xad | 0xbd | 0xb9 | 0xa1 | 0xb1 => {
                    self.lda(&opcode.addressing_mode);
                }

                0xAA => self.tax(),
                0xe8 => self.inx(),
                0x00 => return (),

                /* CLD */ 0xd8 => self.status.remove(CpuFlags::DECIMAL_MODE),

                /* CLI */ 0x58 => self.status.remove(CpuFlags::INTERRUPT_DISABLE),

                /* CLV */ 0xb8 => self.status.remove(CpuFlags::OVERFLOW),

                /* CLC */ 0x18 => self.clc(),

                /* SEC */ 0x38 => self.sec(),

                /* SEI */ 0x78 => self.status.insert(CpuFlags::INTERRUPT_DISABLE),

                /* SED */ 0xf8 => self.status.insert(CpuFlags::DECIMAL_MODE),

                /* PHA */ 0x48 => self.stack_push(self.register_a),

                /* PLA */
                0x68 => {
                    self.pla();
                }

                /* PHP */
                0x08 => {
                    self.php();
                }

                /* PLP */
                0x28 => {
                    self.plp();
                }

                /* ADC */
                0x69 | 0x65 | 0x75 | 0x6d | 0x7d | 0x79 | 0x61 | 0x71 => {
                    self.adc(&opcode.addressing_mode);
                }

                /* SBC */
                0xe9 | 0xe5 | 0xf5 | 0xed | 0xfd | 0xf9 | 0xe1 | 0xf1 => {
                    self.sbc(&opcode.addressing_mode);
                }

                /* AND */
                0x29 | 0x25 | 0x35 | 0x2d | 0x3d | 0x39 | 0x21 | 0x31 => {
                    self.and(&opcode.addressing_mode);
                }

                /* EOR */
                0x49 | 0x45 | 0x55 | 0x4d | 0x5d | 0x59 | 0x41 | 0x51 => {
                    self.eor(&opcode.addressing_mode);
                }

                /* ORA */
                0x09 | 0x05 | 0x15 | 0x0d | 0x1d | 0x19 | 0x01 | 0x11 => {
                    self.ora(&opcode.addressing_mode);
                }

                /* LSR */ 0x4a => self.lsr_accumulator(),

                /* LSR */
                0x46 | 0x56 | 0x4e | 0x5e => {
                    self.lsr(&opcode.addressing_mode);
                }

                /*ASL*/ 0x0a => self.asl_accumulator(),

                /* ASL */
                0x06 | 0x16 | 0x0e | 0x1e => {
                    self.asl(&opcode.addressing_mode);
                }

                /*ROL*/ 0x2a => self.rol_accumulator(),

                /* ROL */
                0x26 | 0x36 | 0x2e | 0x3e => {
                    self.rol(&opcode.addressing_mode);
                }

                /* ROR */ 0x6a => self.ror_accumulator(),

                /* ROR */
                0x66 | 0x76 | 0x6e | 0x7e => {
                    self.ror(&opcode.addressing_mode);
                }

                /* INC */
                0xe6 | 0xf6 | 0xee | 0xfe => {
                    self.inc(&opcode.addressing_mode);
                }

                /* INY */
                0xc8 => self.iny(),

                /* DEC */
                0xc6 | 0xd6 | 0xce | 0xde => {
                    self.dec(&opcode.addressing_mode);
                }

                /* DEX */
                0xca => {
                    self.dex();
                }

                /* DEY */
                0x88 => {
                    self.dey();
                }

                /* CMP */
                0xc9 | 0xc5 | 0xd5 | 0xcd | 0xdd | 0xd9 | 0xc1 | 0xd1 => {
                    self.cmp(&opcode.addressing_mode, self.register_a);
                }

                /* CPY */
                0xc0 | 0xc4 | 0xcc => {
                    self.cmp(&opcode.addressing_mode, self.register_y);
                }

                /* CPX */
                0xe0 | 0xe4 | 0xec => self.compare(&opcode.addressing_mode, self.register_x),

                /* JMP Absolute */
                0x4c => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = mem_address;
                }

                /* JMP Indirect */
                0x6c => {
                    let mem_address = self.mem_read_u16(self.program_counter);
                    // let indirect_ref = self.mem_read_u16(mem_address);
                    //6502 bug mode with with page boundary:
                    //  if address $3000 contains $40, $30FF contains $80, and $3100 contains $50,
                    // the result of JMP ($30FF) will be a transfer of control to $4080 rather than $5080 as you intended
                    // i.e. the 6502 took the low byte of the address from $30FF and the high byte from $3000

                    let indirect_ref = if mem_address & 0x00FF == 0x00FF {
                        let lo = self.mem_read(mem_address);
                        let hi = self.mem_read(mem_address & 0xFF00);
                        (hi as u16) << 8 | (lo as u16)
                    } else {
                        self.mem_read_u16(mem_address)
                    };

                    self.program_counter = indirect_ref;
                }

                /* JSR */
                0x20 => {
                    self.stack_push_u16(self.program_counter + 2 - 1);
                    let target_address = self.mem_read_u16(self.program_counter);
                    self.program_counter = target_address
                }

                /* RTS */
                0x60 => {
                    self.program_counter = self.stack_pop_u16() + 1;
                }

                /* RTI */
                0x40 => {
                    self.status.bits = self.stack_pop();
                    self.status.remove(CpuFlags::BREAK);
                    self.status.insert(CpuFlags::BREAK2);

                    self.program_counter = self.stack_pop_u16();
                }

                /* BNE */
                0xd0 => {
                    self.branch(!self.status.contains(CpuFlags::ZERO));
                }

                /* BVS */
                0x70 => {
                    self.branch(self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BVC */
                0x50 => {
                    self.branch(!self.status.contains(CpuFlags::OVERFLOW));
                }

                /* BPL */
                0x10 => {
                    self.branch(!self.status.contains(CpuFlags::NEGATIV));
                }

                /* BMI */
                0x30 => {
                    self.branch(self.status.contains(CpuFlags::NEGATIV));
                }

                /* BEQ */
                0xf0 => {
                    self.branch(self.status.contains(CpuFlags::ZERO));
                }

                /* BCS */
                0xb0 => {
                    self.branch(self.status.contains(CpuFlags::CARRY));
                }

                /* BCC */
                0x90 => {
                    self.branch(!self.status.contains(CpuFlags::CARRY));
                }

                /* BIT */
                0x24 | 0x2c => {
                    self.bit(&opcode.addressing_mode);
                }

                /* STA */
                0x85 | 0x95 | 0x8d | 0x9d | 0x99 | 0x81 | 0x91 => {
                    self.sta(&opcode.addressing_mode);
                }

                /* STX */
                0x86 | 0x96 | 0x8e => {
                    let addr = self.get_operand_address(&opcode.addressing_mode);
                    self.mem_write(addr, self.register_x);
                }

                /* STY */
                0x84 | 0x94 | 0x8c => {
                    let addr = self.get_operand_address(&opcode.addressing_mode);
                    self.mem_write(addr, self.register_y);
                }

                /* LDX */
                0xa2 | 0xa6 | 0xb6 | 0xae | 0xbe => {
                    self.ldx(&opcode.addressing_mode);
                }

                /* LDY */
                0xa0 | 0xa4 | 0xb4 | 0xac | 0xbc => {
                    self.ldy(&opcode.addressing_mode);
                }

                /* NOP */
                0xea => {
                    //do nothing
                }

                /* TAY */
                0xa8 => {
                    self.register_y = self.register_a;
                    self.update_zero_and_negative_flags(self.register_y);
                }

                /* TSX */
                0xba => {
                    self.register_x = self.stack_pointer;
                    self.update_zero_and_negative_flags(self.register_x);
                }

                /* TXA */
                0x8a => {
                    self.register_a = self.register_x;
                    self.update_zero_and_negative_flags(self.register_a);
                }

                /* TXS */
                0x9a => {
                    self.stack_pointer = self.register_x;
                }

                /* TYA */
                0x98 => {
                    self.register_a = self.register_y;
                    self.update_zero_and_negative_flags(self.register_a);
                }

                _ => todo!(),
            }

            if program_counter_state == self.program_counter {
                self.program_counter += (opcode.len - 1) as u16;
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

    //#[test]
    //fn test_0xa9_lda_immediate_load_data() {
    //    let mut cpu = CPU::new();
    //    cpu.interpret(&[0xa9, 0x05, 0x00]);
    //    assert_eq!(cpu.register_a, 0x05);
    //    assert!(cpu.status & 0b0000_0010 == 0b00);
    //    assert!(cpu.status & 0b1000_0000 == 0);
    //}

    //#[test]
    //fn test_0xa9_lda_zero_flag() {
    //    let mut cpu = CPU::new();
    //    cpu.interpret(&[0xa9, 0x00, 0x00]);
    //    assert!(cpu.status & 0b0000_0010 == 0b10);
    //}

    //#[test]
    //fn test_0xa9_lda_negative_flag() {
    //    let mut cpu = CPU::new();
    //    cpu.interpret(&[0xa9, 0xff, 0x00]);
    //    assert!(cpu.status & 0b1000_0000 == 0b1000_0000);
    //}

    //#[test]
    //fn test_0xaa_tax_move_a_to_x() {
    //    let mut cpu = CPU::new();
    //    cpu.register_a = 10;
    //    cpu.interpret(&[0xaa, 0x00]);

    //    assert_eq!(cpu.register_x, 10)
    //}
    //#[test]
    //fn test_5_ops_working_together() {
    //    let mut cpu = CPU::new();
    //    cpu.interpret(&[0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

    //    assert_eq!(cpu.register_x, 0xc1)
    //}

    //#[test]
    //fn test_inx_overflow() {
    //    let mut cpu = CPU::new();
    //    cpu.register_x = 0xff;
    //    cpu.interpret(&[0xe8, 0xe8, 0x00]);

    //    assert_eq!(cpu.register_x, 1)
    //}

    #[test]
    fn test_0xa9_lda_immediate_load_data() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xa9, 0x05, 0x00]);

        assert_eq!(cpu.register_a, 5);
        assert!(cpu.status.bits() & 0b0000_0010 == 0b00);
        assert!(cpu.status.bits() & 0b1000_0000 == 0);
    }

    #[test]
    fn test_0xaa_tax_move_a_to_x() {
        let mut cpu = CPU::new();
        cpu.register_a = 10;

        cpu.load_and_run(&vec![0xaa, 0x00]);

        assert_eq!(cpu.register_x, 10)
    }

    #[test]
    fn test_5_ops_working_together() {
        let mut cpu = CPU::new();

        cpu.load_and_run(&vec![0xa9, 0xc0, 0xaa, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 0xc1)
    }

    #[test]
    fn test_inx_overflow() {
        let mut cpu = CPU::new();
        cpu.register_x = 0xff;

        cpu.load_and_run(&vec![0xe8, 0xe8, 0x00]);

        assert_eq!(cpu.register_x, 1)
    }

    #[test]
    fn test_lda_from_memory() {
        let mut cpu = CPU::new();
        cpu.mem_write(0x10, 0x55);

        cpu.load_and_run(&vec![0xa5, 0x10, 0x00]);

        assert_eq!(cpu.register_a, 0x55);
    }
}
