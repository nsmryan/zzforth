// NOTES
// Zig seemed to only find a type error when code was executed.
// Does it remove dead code before compiling?
//
// slices and casting is causing me troubles
//
// nice to return both value and error- reduces input parameters and reference use
//
// nice to have type info at compile time, like number of enum variants
//
// zig.vim feedback is very welcome- I don't like long cycles of changes to
// get stuff to compile, so its nice to do it in small pieces along the way.
// Splits development into small writing cycles, syntax cyclings, testing cycles
//
// eval function is a great example of error handling.
// feels a little awkward to put error handling block where it is, but
// there are likely other ways to do it.
//
// no need for setjmp/longjmp, which is nice to see- its not a mechanism I'm that happy
// to use. Zig makes this much easier for about the same result, modulo perhaps the
// error implemntation.
//
// I haven't implemented the tracing capability
//
// I didn't implement the variable sized cells- seems more complex then needed, except
// when size *really* matters.
//
// Zig testing is like Rust testing- its great. Zig seems to compile and run very fast
// for this small project.
// Instead of cargo watch, I've been using entr on WSL on Windows 10.
// occasionally this causes some kind of issue where a file isn't found or it
// gets stuck.
//
// A lot of error handling is not yet checked

const std = @import("std");
const print = std.debug.print;
const assert = std.debug.assert;

// *T pointer to one element
// [*]T pointer to array of elements
// []T slice (pointer with number of elements)
// [_]T slice with unknown number of elements (inferred number)
//

// zfconfig.h
const ZfCell = u32;

const ZfAddr = ZfCell;

const ZF_RSTACK_SIZE: u32 = 4096;
const ZF_DSTACK_SIZE: u32 = 4096;
const ZF_DICT_SIZE: u32 = 4096;

const ZF_NUM_USER_VARS: u32 = 5;

const ZF_MAX_NAME_LEN: u32 = 31;
const ZF_INPUT_BUFFER_SIZE: u32 = 80;

// TODO this could be a bit field instead of mask and shift
const ZF_FLAG_LEN_MASK: u8 = 0x1F;

// zforth.h
const ZfMemSize = enum {
    Var, Cell, U8, U16, U32, S8, S16, S32
};

const ZfInputState = enum {
    Interpret, PassChar, PassWord
};

const ZfSysCallId = enum(ZfCell) {
    Emit, Print, Tell, User = 128
};

// NOTE why does the formatter keep them all on the same line?
const ZfError = error{ NameTooLong, OutOfBounds, InternalError, OutsideMem, DstackUnderrun, DstackOverrun, RstackUnderrun, RstackOverrun, NotAWord, CompileOnlyWord, InvalidSize, DivisionByZero };

// zforth.c
const ZfFlags = enum(u8) {
    None = 0,
    Immediate = 1 << 6,
    Prim = 1 << 5,
};

const ZF_FLAG_MASK: u8 = 0x1F;

const ZfPrim = enum(ZfAddr) {
    Exit,
    Lit,
    Ltz,
    Col,
    Semicol,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Drop,
    Dup,
    Pickr,
    Immediate,
    Peek,
    Poke,
    Swap,
    Rot,
    Jmp,
    Jmp0,
    Tick,
    Comment,
    Pushr,
    Popr,
    Equal,
    Sys,
    Pick,
    Comma,
    Key,
    Lits,
    Len,
    And,
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

const ZForth = struct {
    rstack: [ZF_RSTACK_SIZE]ZfCell = undefined,
    dstack: [ZF_DSTACK_SIZE]ZfCell = undefined,
    dict: [ZF_DICT_SIZE]u8 = undefined,

    input_buffer: [ZF_INPUT_BUFFER_SIZE]u8 = undefined,
    input_len: ZfCell = 0,

    input_state: ZfInputState = ZfInputState.Interpret,
    dsp: ZfAddr = 0,
    rsp: ZfAddr = 0,
    ip: ZfAddr = 0,

    pub fn init() ZForth {
        var zforth = ZForth{};

        std.mem.set(ZfCell, zforth.dstack[0..zforth.dstack.len], 0);
        std.mem.set(ZfCell, zforth.rstack[0..zforth.rstack.len], 0);

        std.mem.set(u8, zforth.dict[0..zforth.dict.len], 0);
        zforth.here().* = ZF_NUM_USER_VARS * @sizeOf(ZfCell);

        std.mem.set(u8, zforth.input_buffer[0..zforth.input_buffer.len], 0);

        return zforth;
    }

    // TODO seems like there should be a nicer way to do this
    pub fn here(self: *ZForth) *align(1) ZfCell {
        var dict_ptr = @ptrCast([*]align(1) ZfCell, &self.dict);
        return &dict_ptr[0];
    }

    pub fn latest(self: *ZForth) *align(1) ZfCell {
        var dict_ptr = @ptrCast([*]align(1) ZfCell, &self.dict);
        return &dict_ptr[1];
    }

    pub fn trace(self: *ZForth) *align(1) ZfCell {
        var dict_ptr = @ptrCast([*]align(1) ZfCell, &self.dict);
        return &dict_ptr[2];
    }

    pub fn compiling(self: *ZForth) *align(1) ZfCell {
        var dict_ptr = @ptrCast([*]align(1) ZfCell, &self.dict);
        return &dict_ptr[3];
    }

    pub fn postpone(self: *ZForth) *align(1) ZfCell {
        var dict_ptr = @ptrCast([*]align(1) ZfCell, &self.dict);
        return &dict_ptr[4];
    }

    pub fn push(self: *ZForth, cell: ZfCell) ZfError!void {
        if (self.dsp == ZF_DSTACK_SIZE) {
            return ZfError.DstackOverrun;
        }

        self.dstack[self.dsp] = cell;
        self.dsp += 1;
    }

    pub fn pop(self: *ZForth) ZfError!ZfCell {
        if (self.dsp == 0) {
            return ZfError.DstackUnderrun;
        }

        self.dsp -= 1;
        return self.dstack[self.dsp];
    }

    pub fn pick(self: *ZForth, n: ZfAddr) ZfError!ZfCell {
        return self.dstack[self.dsp - n - 1];
    }

    pub fn rpush(self: *ZForth, value: ZfCell) ZfError!void {
        if (self.rsp == ZF_RSTACK_SIZE) {
            return ZfError.RstackOverrun;
        }

        self.rstack[self.rsp] = value;
        self.rsp += 1;
    }

    pub fn rpop(self: *ZForth) ZfError!ZfCell {
        if (self.rsp == 0) {
            return ZfError.RstackUnderrun;
        }

        self.rsp -= 1;
        return self.rstack[self.rsp];
    }

    pub fn rpick(self: *ZForth, n: ZfAddr) ZfError!ZfCell {
        return self.rstack[self.rsp - n - 1];
    }

    pub fn dict_put_bytes(self: *ZForth, addr: ZfAddr, buf: []const u8) ZfError!void {
        for (buf) |elem, index| {
            self.dict[addr + index] = elem;
        }
    }

    pub fn dict_get_bytes(self: *ZForth, addr: ZfAddr, buf: []u8) ZfError!void {
        var dict = self.dict[addr .. addr + buf.len];

        for (dict) |elem, index| {
            buf[index] = elem;
        }
    }

    pub fn dict_get_cell(self: *ZForth, addr: ZfAddr) !ZfCell {
        return @ptrCast(*align(1) ZfCell, &self.dict[addr]).*;
    }

    pub fn dict_put_cell(self: *ZForth, addr: ZfAddr, value: ZfCell) ZfError!void {
        @ptrCast(*align(1) ZfCell, &self.dict[addr]).* = value;
    }

    pub fn dict_add_byte(self: *ZForth, byte: u8) ZfError!void {
        const here_ptr = self.here();
        self.dict[self.here().*] = byte;
        self.here().* += @sizeOf(u8);
    }

    pub fn dict_add_cell(self: *ZForth, value: ZfCell) ZfError!void {
        const here_ptr = self.here();
        @ptrCast(*align(1) ZfCell, &self.dict[here_ptr.*]).* = value;
        self.here().* += @sizeOf(ZfCell);
    }

    pub fn dict_add_op(self: *ZForth, op: ZfAddr) ZfError!void {
        const here_ptr = self.here();
        @ptrCast(*align(1) ZfAddr, &self.dict[here_ptr.*]).* = op;
        self.here().* += @sizeOf(ZfAddr);
    }

    pub fn dict_add_lit(self: *ZForth, value: ZfCell) ZfError!void {
        try self.dict_add_op(@enumToInt(ZfPrim.Lit));
        try self.dict_add_cell(value);
    }

    pub fn dict_add_str(self: *ZForth, str: []const u8) ZfError!void {
        try self.dict_put_bytes(self.here().*, str);
        self.here().* += @intCast(ZfCell, str.len);
    }

    pub fn create_word(self: *ZForth, name: []const u8, flags: ZfFlags) ZfError!void {
        if (name.len > ZF_MAX_NAME_LEN) {
            return ZfError.NameTooLong;
        }

        const here_prev: ZfCell = self.here().*;

        try self.dict_add_byte(@intCast(u8, name.len) | @enumToInt(flags));
        try self.dict_add_cell(self.latest().*);
        try self.dict_add_str(name);
        self.latest().* = here_prev;
    }

    pub fn find_word(self: *ZForth, name: []const u8, addr: *ZfAddr, code: *ZfAddr) ZfError!bool {
        var word = self.latest().*;

        while (word != 0) {
            var current_word = word;

            const flags = self.dict[current_word];
            const name_len = flags & ZF_FLAG_LEN_MASK;

            const link = try self.dict_get_cell(current_word + 1);

            const name_offset = current_word + 1 + @sizeOf(ZfCell);
            const word_name = self.dict[name_offset .. name_offset + name_len];
            const words_eql = std.mem.eql(u8, name, word_name);
            if (words_eql) {
                addr.* = word;
                code.* = name_offset + name_len;

                return true;
            }

            word = link;
        }

        return false;
    }

    pub fn make_immediate(self: *ZForth) void {
        const flags = self.dict[self.latest().*];
        self.dict[self.latest().*] = flags | @enumToInt(ZfFlags.Immediate);
    }

    pub fn run(self: *ZForth, input_buf: ?[]const u8) ZfError!?ZfSysCallId {
        var input = input_buf;
        while (self.ip != 0) {
            const ip_orig = self.ip;
            const exe_token = try self.dict_get_cell(self.ip);
            const code = exe_token;

            self.ip += @sizeOf(ZfCell);

            if (code <= @typeInfo(ZfPrim).Enum.fields.len) {
                const syscall = try self.do_prim(@intToEnum(ZfPrim, code), input);

                if (self.input_state != ZfInputState.Interpret) {
                    self.ip = ip_orig;
                    return syscall;
                }
            } else {
                try self.rpush(self.ip);
                self.ip = code;
            }

            // TODO why do we null input here?
            input = null;
        }

        return null;
    }

    pub fn execute(self: *ZForth, addr: ZfAddr) ZfError!?ZfSysCallId {
        self.ip = addr;
        self.rsp = 0;
        try self.rpush(0);
        return try self.run(null);
    }

    pub fn do_prim(self: *ZForth, op: ZfPrim, input: ?[]const u8) ZfError!?ZfSysCallId {
        switch (op) {
            .Exit => {
                self.ip = try self.rpop();
            },

            .Lit => {
                const cell = try self.dict_get_cell(self.ip);
                self.ip += @sizeOf(ZfCell);
                try self.push(cell);
            },

            .Ltz => {
                const value = try self.pop();
                try self.push(@boolToInt(value > 0));
            },

            .Col => {
                if (input) |input_buf| {
                    try self.create_word(input_buf, ZfFlags.None);
                    self.compiling().* = 1;
                } else {
                    self.input_state = ZfInputState.PassWord;
                }
            },

            .Semicol => {
                try self.dict_add_op(@enumToInt(ZfPrim.Exit));
                self.compiling().* = 0;
            },

            .Add => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(value1 + value2);
            },

            .Sub => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(value1 - value2);
            },

            .Mul => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(value1 * value2);
            },

            .Div => {
                // TODO could check for 0, or does Zig do this reasonably?
                const num = try self.pop();
                const denom = try self.pop();
                try self.push(num / denom);
            },

            .Mod => {
                // TODO could check for 0, or does Zig do this reasonably?
                const cell1 = try self.pop();
                const cell2 = try self.pop();
                try self.push(cell2 % cell1);
            },

            .Drop => {
                _ = try self.pop();
            },

            .Dup => {
                const value = try self.pick(0);
                try self.push(value);
            },

            .Pickr => {
                const rpos = try self.pop();
                const value = try self.rpick(rpos);
                try self.push(value);
            },

            .Immediate => {
                self.make_immediate();
            },

            .Peek => {
                // TODO zForth takes a length argument for peek,
                // seemingly due to the variable cell length.
                const addr = try self.pop();
                const cell = try self.dict_get_cell(addr);
                try self.push(cell);
            },

            .Poke => {
                const value = try self.pop();
                const addr = try self.pop();
                try self.dict_put_cell(addr, value);
            },

            .Swap => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(value1);
                try self.push(value2);
            },

            .Rot => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                const value3 = try self.pop();
                try self.push(value2);
                try self.push(value1);
                try self.push(value3);
            },

            .Jmp => {
                const value = try self.dict_get_cell(self.ip);
                self.ip = value;
            },

            .Jmp0 => {
                const value = try self.dict_get_cell(self.ip);
                const tos = try self.pop();
                if (tos == 0) {
                    self.ip = value;
                } else {
                    self.ip += @sizeOf(ZfCell);
                }
            },

            .Tick => {
                const value = try self.dict_get_cell(self.ip);
                try self.push(value);
                self.ip += @sizeOf(ZfCell);
            },

            .Comment => {
                // TODO slightly awkward due to optional
                if (input) |input_buf| {
                    if (input_buf[0] == ')') {
                        self.input_state = ZfInputState.PassChar;
                    }
                } else {
                    self.input_state = ZfInputState.PassChar;
                }
            },

            .Pushr => {
                const value = try self.pop();
                try self.rpush(value);
            },

            .Popr => {
                const value = try self.rpop();
                try self.push(value);
            },

            .Equal => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(@boolToInt(value1 == value2));
            },

            .Sys => {
                const value = try self.pop();
                return @intToEnum(ZfSysCallId, value);

                //self.input_state = self.host_sys(value, input);
                //if (self.input_state != ZfInputState.Interpret) {
                //    try self.push(value);
                //}
            },

            .Pick => {
                const addr = try self.pop();
                const value = try self.pick(addr);
                try self.push(value);
            },

            .Comma => {
                const value = try self.pop();
                try self.dict_add_cell(value);
            },

            .Key => {
                if (input) |buf| {
                    try self.push(buf[0]);
                } else {
                    self.input_state = ZfInputState.PassChar;
                }
            },

            .Lits => {
                // TODO what does lits do?
                const value = try self.dict_get_cell(self.ip);
                try self.push(self.ip);
                try self.push(value);
                self.ip += @sizeOf(ZfCell);
            },

            .Len => {
                @panic("Do we need len in zzForth?");
            },

            .And => {
                const value1 = try self.pop();
                const value2 = try self.pop();
                try self.push(value1 & value2);
            },
        }

        return null;
    }

    pub fn handle_word(self: *ZForth, input: []const u8) ZfError!?ZfSysCallId {
        if (self.input_state == ZfInputState.PassWord) {
            self.input_state = ZfInputState.Interpret;
            return try self.run(input);
        }

        var word_addr: ZfAddr = undefined;
        var code_addr: ZfAddr = undefined;
        const found: bool = try self.find_word(input, &word_addr, &code_addr);

        if (found) {
            const flags: u8 = self.dict[word_addr];

            if (self.compiling().* == 1 and
                (self.postpone().* == 1 or !(flags & @enumToInt(ZfFlags.Immediate) != 0)))
            {
                if (flags & @enumToInt(ZfFlags.Prim) != 0) {
                    const value = try self.dict_get_cell(code_addr);
                    try self.dict_add_op(value);
                } else {
                    try self.dict_add_op(code_addr);
                }

                self.postpone().* = 0;
            } else {
                return try self.execute(code_addr);
            }
        } else {
            const num = try parse_num(input);

            if (self.compiling().* == 1) {
                try self.dict_add_lit(num);
            } else {
                try self.push(num);
            }
        }

        return null;
    }

    pub fn handle_char(self: *ZForth, chr: u8) ZfError!?ZfSysCallId {
        if (self.input_state == ZfInputState.PassChar) {
            self.input_state = ZfInputState.Interpret;
            const chr_buf = [_]u8{chr};

            return try self.run(chr_buf[0..1]);
        } else if (chr != 0 and !std.ascii.isSpace(chr)) {
            if (self.input_len < ZF_INPUT_BUFFER_SIZE - 1) {
                self.input_buffer[self.input_len] = chr;
                self.input_len += 1;
                self.input_buffer[self.input_len] = 0;
            }
            // TODO else return an error that the buffer is full?
            //      or is this handled by the runner?
        } else {
            // chr is 0 or space
            if (self.input_len > 0) {
                const word = self.input_buffer[0..self.input_len];
                self.input_len = 0;
                return try self.handle_word(word);
            }
        }

        return null;
    }

    // NOTE in zForth, these bootstrap functions are optional based on a
    // preprocessor directive
    // NOTE in zForh, a leading underscore is used instead of the 'immediate' flag
    fn add_prim(self: *ZForth, name: []const u8, op: ZfPrim, immediate: bool) ZfError!void {
        try self.create_word(name, ZfFlags.Prim);
        try self.dict_add_op(@enumToInt(op));
        try self.dict_add_op(@enumToInt(ZfPrim.Exit));
        if (immediate) {
            self.make_immediate();
        }
    }

    fn add_uservar(self: *ZForth, name: []const u8, addr: ZfAddr) ZfError!void {
        try self.create_word(name, ZfFlags.None);
        try self.dict_add_lit(addr);
        try self.dict_add_op(@enumToInt(ZfPrim.Exit));
    }

    pub fn bootstrap(self: *ZForth) ZfError!void {
        try self.add_prim("exit", ZfPrim.Exit, false);
        try self.add_prim("lit", ZfPrim.Lit, false);
        try self.add_prim("ltz", ZfPrim.Ltz, false);
        try self.add_prim(":", ZfPrim.Col, true);
        try self.add_prim(";", ZfPrim.Semicol, true);
        try self.add_prim("+", ZfPrim.Add, false);
        try self.add_prim("-", ZfPrim.Sub, false);
        try self.add_prim("*", ZfPrim.Mul, false);
        try self.add_prim("/", ZfPrim.Div, false);
        try self.add_prim("%", ZfPrim.Mod, false);
        try self.add_prim("drop", ZfPrim.Drop, false);
        try self.add_prim("dup", ZfPrim.Dup, false);
        try self.add_prim("pickr", ZfPrim.Pickr, false);
        try self.add_prim("immediate", ZfPrim.Immediate, true);
        try self.add_prim("peek", ZfPrim.Peek, false);
        try self.add_prim("poke", ZfPrim.Poke, false);
        try self.add_prim("swap", ZfPrim.Swap, false);
        try self.add_prim("rot", ZfPrim.Rot, false);
        try self.add_prim("jmp", ZfPrim.Jmp, false);
        try self.add_prim("jmp0", ZfPrim.Jmp0, false);
        try self.add_prim("'", ZfPrim.Tick, false);
        try self.add_prim("(", ZfPrim.Comment, true);
        try self.add_prim("pushr", ZfPrim.Pushr, false);
        try self.add_prim("popr", ZfPrim.Popr, false);
        try self.add_prim("=", ZfPrim.Equal, false);
        try self.add_prim("sys", ZfPrim.Sys, false);
        try self.add_prim("pick", ZfPrim.Pick, false);
        try self.add_prim(",", ZfPrim.Comma, false);
        try self.add_prim("key", ZfPrim.Key, false);
        try self.add_prim("lits", ZfPrim.Lits, false);
        try self.add_prim("len", ZfPrim.Len, false);
        try self.add_prim("&", ZfPrim.And, false);

        try self.add_uservar("here", 0 * @sizeOf(ZfCell));
        try self.add_uservar("latest", 1 * @sizeOf(ZfCell));
        try self.add_uservar("trace", 2 * @sizeOf(ZfCell));
        try self.add_uservar("compiling", 3 * @sizeOf(ZfCell));
        try self.add_uservar("postpone", 4 * @sizeOf(ZfCell));
    }

    pub fn eval(self: *ZForth, input: []const u8) ZfError!?ZfSysCallId {
        for (input) |chr| {
            // handle each character, but if there is an error
            // then reset the system and return it.
            const syscall = self.handle_char(chr) catch |err| {
                self.compiling().* = 0;
                self.rsp = 0;
                self.dsp = 0;
                return err;
            };

            if (syscall) |id| {
                return id;
            }
        }

        return null;
    }
};

fn parse_num(input: ?[]const u8) ZfError!ZfCell {
    // NOTE radix could be part of the system state, as in some forths
    if (input) |buf| {
        return std.fmt.parseInt(ZfCell, buf, 10) catch {
            return ZfError.NotAWord;
        };
    } else {
        return ZfError.InternalError;
    }
}

test "zforth get/set dict" {
    var zforth = ZForth.init();

    const src = [_]u8{ 1, 2, 3, 4, 5 };
    const offset = 1;

    try zforth.dict_put_bytes(offset, src[0..src.len]);
    for (src) |elem, index| {
        assert(elem == zforth.dict[index + offset]);
    }

    var dst: [src.len]u8 = undefined;
    try zforth.dict_get_bytes(offset, dst[0..dst.len]);

    for (src) |elem, index| {
        assert(elem == dst[index]);
    }
}

test "zforth add/get cell" {
    var zforth = ZForth.init();

    const value = 123;
    const orig_here = zforth.here().*;
    assert(orig_here == ZF_NUM_USER_VARS * @sizeOf(ZfCell));
    try zforth.dict_add_cell(value);

    assert((try zforth.dict_get_cell(orig_here)) == value);

    assert(zforth.here().* == orig_here + @sizeOf(ZfCell));
    assert((try zforth.dict_get_cell(orig_here)) == value);
}

test "zforth add word" {
    var zforth = ZForth.init();

    const name = "test";
    const flag = ZfFlags.Prim;
    try zforth.create_word(@as([]const u8, name), flag);

    // check that the flags take 1 byte and are the OR of the length and flags
    assert(zforth.dict[zforth.latest().*] == (name.len + @enumToInt(flag)));

    // check that the name is in the dictionary
    assert(zforth.dict[zforth.latest().* + @sizeOf(ZfCell) + 1] == name[0]);
    assert(zforth.dict[zforth.latest().* + @sizeOf(ZfCell) + 2] == name[1]);
    assert(zforth.dict[zforth.latest().* + @sizeOf(ZfCell) + 3] == name[2]);
    assert(zforth.dict[zforth.latest().* + @sizeOf(ZfCell) + 4] == name[3]);

    // check that we can find the word
    var addr: ZfAddr = undefined;
    var code: ZfAddr = undefined;
    assert(try zforth.find_word(name, &addr, &code));
    assert(code == addr + 1 + @sizeOf(ZfCell) + name.len);
    // if the word isn't in the dictionary, we don't find it
    assert(!try zforth.find_word("notname", &addr, &code));

    zforth.make_immediate();
    assert(zforth.dict[addr] & @enumToInt(ZfFlags.Immediate) != 0);
}

test "zforth simple eval" {
    var zforth = ZForth.init();

    try zforth.bootstrap();

    // TODO the final space is currently necessary
    const prog = "1 2 + ";
    _ = try zforth.eval(prog);

    const result = try zforth.pop();
    assert(result == 3);
}

test "zforth complex eval" {
    var zforth = ZForth.init();

    try zforth.bootstrap();

    // TODO the final space is currently necessary
    const prog = ": add_one 1 + ; 10 add_one ";
    _ = try zforth.eval(prog);

    const result = try zforth.pop();
    assert(result == 11);
}
