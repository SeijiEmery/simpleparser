#!/usr/bin/env rdmd
module simpleparser;
import std.file;
import std.path;
import std.stdio;
import std.exception: basicExceptionCtors, enforce;
import std.format;
import std.variant;
import std.algorithm;
import std.regex;

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
    /// tagged union types
    enum Type {
        // note that these are -structural- types
        NONE         = 0x00,
        LITERAL      = 0x01,    // literal (ie. owns a string)
        TREF         = 0x02,    // term reference (owns a string; could be changed to a direct size_t index in the future)
        SEQUENCE     = 0x04,    // sequence of multiple terms
        EITHER       = 0x08,    // match -any- of these term(s)

        // and these are type -additions-, ie. a term of one
        // of the existing types may also:
        ZERO_OR_ONE  = 0x10,    // may match nothing (ie. '?') 
        ONE_OR_MORE  = 0x20,    // may match multiple arguments (ie. '+')
        ZERO_OR_MORE = 0x30,    // may match nothing, something, or multiple args (ie. '*')

        // note that the layout here is intentional, ie. 
        //  0xf & type  => base type (NONE | LITERAL | TREF | SEQUENCE | EITHER)
        //  type & 0xf0 => type additions (NONE | ZERO_OR_ONE | ONE_OR_MORE | ZERO_OR_MORE)
        //  type >> 4   => 0x0 (N/A) | 0x1 (?) | 0x2 (+) | 0x3 (*)
    }

    /// tagged union of all term types. 
    /// small atom struct: has size in the range of ~3 * size_t.sizeof bytes, fwiw
    struct Term {
        Type type;
        union {
            struct { Term*[] terms = null; } // SEQUENCE | EITHER
            struct { Term*  term; } // unused...?
            struct { string tref; } // TREF
            struct { string text; } // LITERAL
        }
    }

    /// file name for printing purposes
    string fileName;

    /// list of all patterns (our defn is Pattern = PatternName => Term)
    Term*[string] patterns;

    //
    // memory management
    // 

    ~this () { patterns.clear; termAllocator.deallocateAll; }

    import std.experimental.allocator;
    import std.experimental.allocator.building_blocks.allocator_list;
    import std.experimental.allocator.building_blocks.region;
    import std.experimental.allocator.mallocator;
    import std.algorithm;

    alias TermAllocator = AllocatorList!((n) => Region!Mallocator(max(n, n * 768)));//1024)));
    TermAllocator termAllocator;    // use a specific allocator for term memory for high locality
    Term*[] allocatedTerms;         // list of all allocated terms for debugging purposes

    Term* makeTerm (Type type) {    // allocate everything from here
        //writefln("create %s", type); 
        auto term = termAllocator.make!Term(type);
        allocatedTerms ~= term;
        return term;
    }

    // term ctors

    /// construct + insert a new pattern declaration, which implicitely is an EITHER term
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
    /// construct a term reference term (ie. string name => term pattern lookup)
    /// note that our behavior here is a bit special, and quite deliberate:
    /// - we pre-allocate empty terms for term refs that haven't been defined yet
    /// - this makes finding undefined terms trivial (just sweep for None types in patterns)
    /// - it also guarantees that any term reference lookup from an inserted / allocated
    ///   tref will always return a valid Term* pointer, and thus we should -never-
    ///   need to check for null on the result of `patterns[tref.name]`
    Term* tref (string name) {
        if (name !in patterns) {
            patterns[name] = makeTerm(Type.NONE);
        }
        auto term = makeTerm(Type.TREF);
        term.tref = name;
        return term;
    }
    /// construct a new string literal term
    Term* literal (string text) {
        auto term = makeTerm(Type.LITERAL);
        term.text = text;
        return term;
    }
    /// construct a new linear sequence of terms term
    Term* seq (Term*[] terms) {
        auto term = makeTerm(Type.SEQUENCE);
        term.terms = terms;
        return term;
    }
    /// construct a new either A / B / C / ... term
    Term* either (Term*[] terms) {
        auto term = makeTerm(Type.EITHER);
        term.terms = terms;
        return term;
    }
    /// "construct" (actually just modify) a one-or-more term (ie '+')
    Term* oneOrMore (Term* term) { term.type |= Type.ONE_OR_MORE; return term; }

    /// "construct" (actually just modify) a zero-or-one term (ie '?')
    Term* optional  (Term* term) { term.type |= Type.ZERO_OR_ONE; return term; }

    /// "construct" (actually just modify) a zero-or-more term (ie '*')
    Term* zeroOrMore (Term* term) { term.type |= Type.ZERO_OR_MORE; return term; }

    //
    // pretty printing
    //

    /// pretty print ebnf contents in a similar-ish format to d's ebnf notation,
    /// but with 'MISSING REFERENCE' inserted for missing / referenced-but-not-defined named terms / patterns
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

    //
    // memory debug printing
    //

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

    //
    // utility functions
    //

    /// get a lazy list of undefined term(s) in this EBNF file
    auto undefinedTerms () {
        import std.algorithm: filter;
        return patterns.keys.filter!((key) => patterns[key].type == Type.NONE);
    }
}

/// read an ebnf file using D-like ebnf syntax:
/// - terms are separated using '|'
/// - term declarations are whitespace insensitive (but pretty printed), and always terminated with ';'
EBNF readEBNF (string fileName, string text) {
    auto ebnf = EBNF(fileName);
    //ebnf.decl("addExpression", [
    //    ebnf.tref("mulExpression"),
    //    ebnf.seq([
    //        ebnf.tref("addExpression"),
    //        ebnf.oneOrMore(
    //            ebnf.either([ ebnf.literal("+"), ebnf.literal("-"), ebnf.literal("~") ])),
    //        ebnf.tref("mulExpression")
    //    ])
    //]);

    //try {
        while (text.length) {
            ebnf.parseDecl(text);
        }
    //} catch (ParseException e) {
    //    writefln("%s", e);
    //}

    ebnf.dumpTermMemory();
    writeln();

    import std.array;
    auto missingTerms = ebnf.undefinedTerms.array;
    writefln("%s undefined term(s): %s", missingTerms.length, missingTerms.join(", "));
    //writefln("ok");

    return ebnf;
}

string sliceUpTo (string s, size_t n) { return n < s.length ? s[0..n] : s; }

string parseId (ref string input) {
    //writefln("parseId '%s'", input.sliceUpTo(20));

    auto match = input.match(ctRegex!r"^\s*([a-zA-Z_][a-zA-Z_\-0-9]*)");
    enforce!ParseException(match, format("expected identifier, got '%s'", input.sliceUpTo(20)));
    input = match.post;
    return match.captures[1];
}

alias Term = EBNF.Term;

void parseDecl (ref EBNF ebnf, ref string input) {
    //writefln("parseDecl '%s'", input.sliceUpTo(20));

    auto name = input.parseId;
    auto match = input.matchFirst(ctRegex!r"^\s*\:");
    enforce!ParseException(match, 
        format("expected ':' after id in pattern decl, but got '%s'",
            input.sliceUpTo(20)));
    input = match.post;

    Term*[] terms;
    do {
        terms ~= ebnf.parseTermSeq(input);
        match = input.matchFirst(ctRegex!r"^\s*([\|;])");
        enforce!ParseException(match,
            format("expected '|' or ';', got '%s'", input.sliceUpTo(20)));
        input = match.post;
        if (match[1] == ";") break;
    } while (true);

    ebnf.decl(name, terms);
}

Term* parseLiteral (ref EBNF ebnf, ref string input) {
    //writefln("parseLiteral '%s'", input.sliceUpTo(20));

    auto split = input.findSplit("'");
    enforce(split[1].length,
        format("missing `'` at end of string literal starting with '%s'", input.sliceUpTo(20)));
    input = split[2];
    //writefln(" => `%s` `%s` `%s`", split[0], split[1], split[2].sliceUpTo(20));
    return ebnf.literal(split[0]);
}
Term* parseTRef (ref EBNF ebnf, ref string input) {
    //writefln("parseTRef '%s'", input.sliceUpTo(20));

    return ebnf.tref(input.parseId);
}
Term* parseTermSeq (ref EBNF ebnf, ref string input) {
    //writefln("parseTermSeq '%s'", input.sliceUpTo(20));

    Term*[] terms;
    do {
        auto match = input.match(ctRegex!r"^\s*(['\(\);])");
        if (!match) { terms ~= ebnf.parseTRef(input); }
        else if (match.captures[1] == "'") { input = match.post; terms ~= ebnf.parseLiteral(input); }
        else if (match.captures[1] == "(") { 
            input = match.post; 
            terms ~= ebnf.parseTermSeq(input);
            match = input.match(ctRegex!r"^\s*([\);])");
            enforce!ParseException(match,
                format("expected ')' or ';', got '%s'", input.sliceUpTo(20)));
            if (match.captures[1] == ")") { input = match.post; }
            else if (match.captures[1] == ";") break;
            else assert(0);
        } else if (match.captures[1] == ")" || match.captures[1] == ";") break;
        else assert(0);

        //writefln("finished parsing term; at '%s'", input.sliceUpTo(20));

        assert(terms.length);
        match = input.match(ctRegex!r"^\s*([\*\+\?])");
        if (!match) {}
        else if (match.captures[1] == "*") { input = match.post; terms[$-1] = ebnf.zeroOrMore(terms[$-1]); }
        else if (match.captures[1] == "+") { input = match.post; terms[$-1] = ebnf.oneOrMore(terms[$-1]); }
        else if (match.captures[1] == "?") { input = match.post; terms[$-1] = ebnf.optional(terms[$-1]); }
        else assert(0);

        match = input.match(ctRegex!r"^\s*([\|])");
        if (match && match.captures[1] == "|") { input = match.post; goto parseEither; }
    } while (1);
    enforce!ParseException(terms.length,
        format("expected term(s), got '%s'", input.sliceUpTo(20)));
    return terms.length > 1 ? ebnf.seq(terms) : terms[0];
parseEither:
    assert(terms.length);
    if (terms.length > 1) terms = [ ebnf.seq(terms) ];
    terms ~= ebnf.parseTermSeq(input);
    while (true) {
        auto match = input.match(ctRegex!r"^\s*(\|)");
        if (match) {
            input = match.post;
            terms ~= ebnf.parseTermSeq(input);
        } else break;
    }
    assert(terms.length > 1);
    return ebnf.either(terms);
}
