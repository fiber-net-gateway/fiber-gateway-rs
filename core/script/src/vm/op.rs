/// Bytecode opcodes mirrored from the Java `Code` interface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum OpCode {
    Noop = 1,
    LoadConst = 3,
    LoadRoot = 4,
    Dump = 5,
    Pop = 6,
    LoadVar = 7,
    StoreVar = 8,
    NewObject = 10,
    NewArray = 11,
    ExpObject = 12,
    ExpArray = 13,
    PushArray = 14,
    IdxGet = 15,
    IdxSet = 16,
    IdxSet1 = 17,
    PropGet = 18,
    PropSet = 19,
    PropSet1 = 20,
    BopPlus = 25,
    BopMinus = 26,
    BopMultiply = 27,
    BopDivide = 28,
    BopMod = 29,
    BopMatch = 30,
    BopLt = 31,
    BopLte = 32,
    BopGt = 33,
    BopGte = 34,
    BopEq = 35,
    BopSeq = 36,
    BopNe = 37,
    BopSne = 38,
    BopIn = 39,
    UnaryPlus = 43,
    UnaryMinus = 44,
    UnaryNeg = 45,
    UnaryTypeof = 46,
    CallFunc = 50,
    CallFuncSpread = 51,
    CallAsyncFunc = 52,
    CallAsyncFuncSpread = 53,
    CallConst = 56,
    CallAsyncConst = 57,
    Jump = 60,
    JumpIfFalse = 61,
    JumpIfTrue = 62,
    IterateInto = 65,
    IterateNext = 66,
    IterateKey = 67,
    IterateValue = 68,
    IntoCatch = 69,
    ThrowExp = 75,
    EndReturn = 76,
}

impl OpCode {
    pub fn from_byte(byte: u8) -> Option<Self> {
        Some(match byte {
            1 => Self::Noop,
            3 => Self::LoadConst,
            4 => Self::LoadRoot,
            5 => Self::Dump,
            6 => Self::Pop,
            7 => Self::LoadVar,
            8 => Self::StoreVar,
            10 => Self::NewObject,
            11 => Self::NewArray,
            12 => Self::ExpObject,
            13 => Self::ExpArray,
            14 => Self::PushArray,
            15 => Self::IdxGet,
            16 => Self::IdxSet,
            17 => Self::IdxSet1,
            18 => Self::PropGet,
            19 => Self::PropSet,
            20 => Self::PropSet1,
            25 => Self::BopPlus,
            26 => Self::BopMinus,
            27 => Self::BopMultiply,
            28 => Self::BopDivide,
            29 => Self::BopMod,
            30 => Self::BopMatch,
            31 => Self::BopLt,
            32 => Self::BopLte,
            33 => Self::BopGt,
            34 => Self::BopGte,
            35 => Self::BopEq,
            36 => Self::BopSeq,
            37 => Self::BopNe,
            38 => Self::BopSne,
            39 => Self::BopIn,
            43 => Self::UnaryPlus,
            44 => Self::UnaryMinus,
            45 => Self::UnaryNeg,
            46 => Self::UnaryTypeof,
            50 => Self::CallFunc,
            51 => Self::CallFuncSpread,
            52 => Self::CallAsyncFunc,
            53 => Self::CallAsyncFuncSpread,
            56 => Self::CallConst,
            57 => Self::CallAsyncConst,
            60 => Self::Jump,
            61 => Self::JumpIfFalse,
            62 => Self::JumpIfTrue,
            65 => Self::IterateInto,
            66 => Self::IterateNext,
            67 => Self::IterateKey,
            68 => Self::IterateValue,
            69 => Self::IntoCatch,
            75 => Self::ThrowExp,
            76 => Self::EndReturn,
            _ => return None,
        })
    }
}

/// Raw 32-bit encoded instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Instruction(pub u32);

impl Instruction {
    pub fn opcode(self) -> Option<OpCode> {
        OpCode::from_byte((self.0 & 0xFF) as u8)
    }

    pub fn operand_u8(self) -> u8 {
        (self.0 >> 8) as u8
    }

    pub fn operand_u16(self) -> u16 {
        (self.0 >> 8) as u16
    }

    pub fn operand_u24(self) -> u32 {
        self.0 >> 8
    }
}
