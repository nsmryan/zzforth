// zfconfig.h
const ZfAddr = u32;

const ZF_RSTACK_SIZE: u32 = 4096;
const ZF_DSTACK_SIZE: u32 = 4096;
const ZF_DICT_SIZE: u32 = 4096;

// zforth.h
// NOTE this should be an 'error' type
const ZfResult = enum {
    ZF_OK,
    ZF_ABORT_INTERNAL_ERROR,
    ZF_ABORT_OUTSIDE_MEM,
    ZF_ABORT_DSTACK_UNDERRUN,
    ZF_ABORT_DSTACK_OVERRUN,
    ZF_ABORT_RSTACK_UNDERRUN,
    ZF_ABORT_RSTACK_OVERRUN,
    ZF_ABORT_NOT_A_WORD,
    ZF_ABORT_COMPILE_ONLY_WORD,
    ZF_ABORT_INVALID_SIZE,
    ZF_ABORT_DIVISION_BY_ZERO
};

const ZfMemSize = enum {
    ZF_MEM_SIZE_VAR,
    ZF_MEM_SIZE_CELL,
    ZF_MEM_SIZE_U8,
    ZF_MEM_SIZE_U16,
    ZF_MEM_SIZE_U32,
    ZF_MEM_SIZE_S8,
    ZF_MEM_SIZE_S16,
    ZF_MEM_SIZE_S32
};

const ZfInputState = enum {
    ZF_INPUT_INTERPRET,
    ZF_INPUT_PASS_CHAR,
    ZF_INPUT_PASS_WORD
};

const ZfSysCallId = enum {
    ZF_SYSCALL_EMIT,
    ZF_SYSCALL_PRINT,
    ZF_SYSCALL_TELL,
    ZF_SYSCALL_USER = 128
};


// zforth.c
const ZF_FLAGS = enum(u8) {
    IMMEDIATE = 1 << 6,
    PRIM = 1 << 5,
};

const ZF_FLAG_MASK: u8 = 0x1F;

const ZF_PRIM = enum(u8) {
    PRIM_EXIT,
    PRIM_LIT,
    PRIM_LTZ,
    PRIM_COL,
    PRIM_SEMICOL,
    PRIM_ADD,
    PRIM_SUB,
    PRIM_MUL,
    PRIM_DIV,
    PRIM_MOD,
    PRIM_DROP,
    PRIM_DUP,
    PRIM_PICKR,
    PRIM_IMMEDIATE,
    PRIM_PEEK,
    PRIM_POKE,
    PRIM_SWAP,
    PRIM_ROT,
    PRIM_JMP,
    PRIM_JMP0,
    PRIM_TICK,
    PRIM_COMMENT,
    PRIM_PUSHR,
    PRIM_POPR,
    PRIM_EQUAL,
    PRIM_SYS,
    PRIM_PICK,
    PRIM_COMMA,
    PRIM_KEY,
    PRIM_LITS,
    PRIM_LEN,
    PRIM_AND,
    //PRIM_COUNT
};


//const prim_names: [_][_]u8 = {
//    "exit",    "lit",        "<0",    ":",     "_;",        "+",
//    "-",       "*",          "/",     "%",     "drop",      "dup",
//    "pickr",   "_immediate", "@@",    "!!",    "swap",      "rot",
//    "jmp",     "jmp0",       "'",     "_(",    ">r",        "r>",
//    "=",       "sys",        "pick",  ",,",    "key",       "lits",
//    "##",      "&"
//    };

//const uservar_names: [][]u8 = {
//    "h", "latest", "trace", "compiling", "postpone",
//};

const ZCell = u32;

const ZForth = struct {
    rstack: [ZF_RSTACK_SIZE]ZCell,
    dstack: [ZF_DSTACK_SIZE]ZCell,
    dict: [ZF_DICT_SIZE]u8,

    input_state: ZfInputState = ZfInputState.ZF_INPUT_INTERPRET,
    dsp: ZfAddr = 0,
    rsp: ZfAddr = 0,
    ip: ZfAddr = 0,

    user_var: []u8,

    pub fn init() ZForth {
        return ZForth { };
    }

    pub fn push(self: *ZForth, cell: ZfCell) !void {
        self.dstack[self.dsp];
        self.dsp += 1;
    }

    pub fn pop(self: *ZForth) !ZfCell {
        self.dsp -= 1;
        return self.dstack[self.dsp];
    }

    pub fn pick(self: *ZForth, n: ZfAddr) !ZfCell {
        return self.dstack[self.dsp - n - 1];
    }

    pub fn rpush(self: *ZForth, value: ZfCell) !void {
        self.rstack[self.rsp] = value;
        self.rsp += 1;
    }
};

test "zforth" {
    var zforth = ZForth.init();
}

pub fn main() void {
}
