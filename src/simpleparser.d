#!/usr/bin/env rdmd
module simpleparser;
import std.file;
import std.path;
import std.stdio;
import std.exception: basicExceptionCtors, enforce;
import std.format;
import std.variant;

int main (string[] args) {
    void processFile (string file) {
        writefln("%s", file);
        auto bnf = readEBNF(file, file.readText);
        writefln("%s", bnf);
        writeln();
    }
    if (args.length > 1) {
        foreach (file; args[1..$]) {
            processFile(file);
        }
    } else {
        processFile("dlang.ebnf");
    }
    import core.stdc.stdlib;
    exit(0);
    return 0;
}

class ParseException : Exception {
    mixin basicExceptionCtors;
}

struct EBNF {
    enum Type {
        NONE         = 0x00,
        LITERAL      = 0x01, 
        TREF         = 0x02,
        SEQUENCE     = 0x04,
        EITHER       = 0x08,
        ZERO_OR_ONE  = 0x10,
        ZERO_OR_MORE = 0x30,
        ONE_OR_MORE  = 0x20,
    }
    struct Term {
        Type type;
        union {
            struct { Term*[] terms = null; }
            struct { Term*  term; }
            struct { string tref; }
            struct { string text; }
        }
    }
    string fileName;
    Term*[string] patterns;

    ~this () { patterns.clear; termAllocator.deallocateAll; }


    //
    // memory management
    // 

    import std.experimental.allocator;
    import std.experimental.allocator.building_blocks.allocator_list;
    import std.experimental.allocator.building_blocks.region;
    import std.experimental.allocator.mallocator;
    import std.algorithm;

    alias TermAllocator = AllocatorList!((n) => Region!Mallocator(max(n, n * 768)));//1024)));
    TermAllocator termAllocator;    // use a specific allocator for term memory for high locality
    Term*[] allocatedTerms;         // list of all allocated terms for debugging purposes

    Term* makeTerm (Type type) {
        //writefln("create %s", type); 
        auto term = termAllocator.make!Term(type);
        allocatedTerms ~= term;
        return term;
    }
    Term* decl (string name, Term*[] options) {
        enforce(name !in patterns || patterns[name].type == Type.NONE,
            format("already exists: '%s'", name));
        auto term = name in patterns ? 
            patterns[name] : 
            (patterns[name] = makeTerm(Type.NONE));
        term.type = Type.EITHER;
        term.terms = options;
        return term;
    }
    Term* literal (string text) {
        auto term = makeTerm(Type.LITERAL);
        term.text = text;
        return term;
    }
    Term* tref (string name) {
        if (name !in patterns) {
            patterns[name] = makeTerm(Type.NONE);
        }
        auto term = makeTerm(Type.TREF);
        term.tref = name;
        return term;
    }
    Term* seq (Term*[] terms) {
        auto term = makeTerm(Type.SEQUENCE);
        term.terms = terms;
        return term;
    }
    Term* either (Term*[] terms) {
        auto term = makeTerm(Type.EITHER);
        term.terms = terms;
        return term;
    }
    Term* oneOrMore (Term* term) { term.type |= Type.ONE_OR_MORE; return term; }
    Term* optional  (Term* term) { term.type |= Type.ZERO_OR_ONE; return term; }
    Term* zeroOrMore (Term* term) { term.type |= Type.ZERO_OR_MORE; return term; }

    void toString (scope void delegate(const(char)[]) sink) {
        import std.conv: to;
        sink("ebnf '"); sink(fileName); sink("', ");
        sink(patterns.length.to!string); sink(" pattern(s):");
        foreach (k; patterns.keys) {
            sink("\n  "); sink(k);
            auto t = patterns[k];
            if (t.type != Type.NONE) {
                sink(":\n      ");
                foreach (i, term; patterns[k].terms) {
                    if (i) sink("\n    | ");
                    printRecursive(term, sink);
                }
                sink("\n    ;");
            } else {
                sink(": MISSING REFERENCE");
            }
        }
    }
    private void printRecursive(Term* term, scope void delegate(const(char)[]) sink) {
        final switch (term.type & 0xF) {
            case Type.NONE: sink("missing reference..."); return;
            case Type.LITERAL: sink("'"); sink(term.text); sink("'"); break;
            case Type.TREF: sink(term.tref); break;
            case Type.SEQUENCE:
                foreach (i, child; term.terms) { 
                    if (i) sink(" ");
                    printRecursive(child, sink); 
                } break;
            case Type.EITHER: sink("(");
                foreach (i, child; term.terms) { 
                    if (i) sink(" | "); 
                    printRecursive(child, sink); 
                } sink(")"); break;
        }
        final switch (term.type & 0xF0) {
            case Type.NONE: break;
            case Type.ZERO_OR_MORE: sink("*"); break;
            case Type.ZERO_OR_ONE: sink("?"); break;
            case Type.ONE_OR_MORE: sink("+"); break;
        }
    }

    void dumpTermMemory () {
        writefln("%s term atom(s) allocated:", allocatedTerms.length);
        foreach (term; allocatedTerms) {
            auto mem = (cast(ubyte*)term)[0..Term.sizeof];

            static immutable char[] lut = [ ' ', '?', '+', '*' ];
            writef("  %x | ", term);
            foreach (b; mem) writef("%2x", b);
            writef("%10s %c ", cast(Type)(term.type & 0xF), lut[term.type >> 4]);
            final switch (term.type & 0xf) {
                case Type.NONE: break;
                case Type.LITERAL: writef("'%s' (string memory at %x)", term.text, term.text.ptr); break;
                case Type.TREF:    writef("'%s' at %x (string memory at %x)", term.text, patterns[term.text], term.text.ptr); break;
                case Type.SEQUENCE: writef(" => [ "); foreach (t; term.terms) { writef("%x ", t); } writef("] (array memory at %x)", term.terms.ptr); break;
                case Type.EITHER: writef(" => [ "); foreach (t; term.terms) { writef("%x ", t); } writef("] (array memory at %x)", term.terms.ptr); break;
            }
            writeln();
        }
    }

}
EBNF readEBNF (string fileName, string text) {
    auto ebnf = EBNF(fileName);
    ebnf.decl("addExpression", [
        ebnf.tref("mulExpression"),
        ebnf.seq([
            ebnf.tref("addExpression"),
            ebnf.oneOrMore(
                ebnf.either([ ebnf.literal("+"), ebnf.literal("-"), ebnf.literal("~") ])),
            ebnf.tref("mulExpression")
        ])
    ]);

    ebnf.dumpTermMemory();
    writeln();
    //writefln("ok");

    return ebnf;
}


