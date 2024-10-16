// https://bugzmanov.github.io/nes_ebook/chapter_3_1.html
pub struct CPU {
    pub register_a: u8,
    pub register_x: u8,
    pub status: u8,
    pub program_counter: u16,
}

impl CPU {
    pub fn new() -> Self {
        Self {
            register_a: 0,
            register_x: 0,
            status: 0,
            program_counter: 0,
        }
    }

    fn lda(&mut self, value: u8) {
        self.register_a = value;
    }

    fn tax(&mut self, value: u8) {
        self.register_x = self.register_a;
    }

    pub fn interpret(&mut self, program: &[u8]) {
        // I don't think we need this
        // self.program_counter = 0;

        loop {
            let opscode = program[self.program_counter as usize];
            self.program_counter += 1;

            let pc = self.program_counter as usize;

            match opscode {
                0xA9 => {
                    let param = program[pc];
                    self.program_counter += 1;
                    self.lda(param);
                }
                0xAA => self.tax(),
                0x00 => return,
            }
        }
    }
}

fn main() {
    println!("Hello, world!");
}
