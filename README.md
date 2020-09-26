# zzForth
This repository contains an implementation of [zForth](https://github.com/zevv/zForth) in the
[Zig programming language](https://ziglang.org).

This translation is mostly faithful, but makes some changes to the feature set compared
to the original zForth.

I did this translation to learn Zig because I like Forth, and I think its interesting to
consider embedding a simple language into other application, but this is just a
short learning project.

## Building and Running
To run the tests, simply use:
```bash
zig test zzforth.zig
```

and to build
```bash
zig build main.zig
```

## Differences from zForth
I did not implment the tracing capability- I think its useful and interesting,
but I was just trying to get the thing to work first.

I did not implement the variable sized cells. This is an interesting approach,
and I'm sure it makes the system much smaller, but the complexity wasn't worth
it for me.

I haven't yet made the system as configurable as zForth, such as cell type. 
You can always modify the code to get this to work, but in Zig we could have
even a better system by providing extra comptime arguments.

### User Defined Cell Type
I didn't not let the user define the cell type. In zForth the cell type is a 
type in the zfconf.h file. This could be provided as a comptime argument to
the ZForth struct, but then you would need to figure out how to parse numbers,
either by providing a wrapping type or providing the parser as an argument.


Zig also doesn't want you to index an array with a signed type, so I had to
do a good bit of casting to get this to work.

## Notes on Zig
I learned a good deal of Zig while writing this:

### Array/Pointer Types
The different pointer types take some time to get used to. I like the precision
they imply- there are many use-cases for pointers and array, and Zig lets you
express which one you intend rather then using pointers for everything.

The disadvantage has been that I often find I have to cast or convert between types,
but I'm okay with that kind of thing if it makes the code more correct.

### Error Handling
The error handling is quite nice. I am used to doing all of this manually in C, but
having the simple error handling cases bulit-in led me to handle errors naturally.
There is definitely some danger in just writing all code to short-circuit on errors,
but I don't expect the language to do the thinking, only to provide good mechanisms.
Zig seems to provide the error handling I do anyway, but built-in so it is concise
and takes many fewer lines of code then in C.


The zForth code uses setjmp/longjmp, and I'm happy to see that in Zig I can do
the simple error handling of returning error codes. There are also places in
zForth where C requires output arguments to return information when an error
code can also be returned, where Zig builds in an Either/Result type so the
return type describes both the error and the return value without resorting
to output parameters.

### Compile Time Type Info
One of my favorite things about Zig is access to type information at compile time.
In zForth, the zf\_prim type ends in PRIM\COUNT variants, which gives the number of 
enum variants. This does work, but only when the compile assigns values and 0 is
valid. In Zig we can just ask for the number of variants directly, regardless of their
values.

This looks like "@typeInfo(ZfPrim).Enum.fields.len", asking for the length of the
array of fields of the Enum ZfPrim.

### Zig Formatting
I am personally a fan of language formatters that keep all code consistent. I don't
necessarily agree with all formatting decisions in Zig, but I also don't care about
formatting and I would prefer all code to follow the same patterns to 
help with mental pattern matching of code, and to avoid friction between
codebases, manual work, and any trivial discussions.

### Compile Time Asserts
I very much like the ease of compile time asserts in Zig. I didn't make that
much use of them, but its nice to be able to express complex logic asserts.
I could see this being useful to add a bit of correctness in more complex cases.

### Development Cycle
I had a good time with the genernal development cycle of Zig. The zig.vim plugin
for vim makes formatting errors immediately apparent so I can fix them as I go, which I
definitely prefer. I used [entr](https://github.com/eradman/entr) to test immediately
on every save:
```bash
echo "zzforth.zig" | entr -c zig test zzforth.zig
```
This occurs fast enough that it seems almost instant, which is very nice. I hear
Zig is supposed to get faster in the future, with the self-hosted change, which
may make this even better.


Writing tests alongside functions, and running them immediately and every time, is
a great way to develop. I do this in Rust as well, and sometimes set it up manually
in C, and I vastly prefer it to manually testing or even manually running tests.


## Problems
I did have some problems with the current version of Zig (0.6.0+0962cc5a3):

It seems that Zig does not compile code that is not used. This means that unused
code is a landmine that might not compile, or might have compiled once but does
not anymore. I don't know if this is intentional, but it is quite surprising.


There are occasional errors in zigfmt and running the tests. Perhaps some kind
of race condition or inconsistent state occurred in at least two ways. All
I had to do was run them again, and I've had similar problems with Rust, but
its still worth noting.


