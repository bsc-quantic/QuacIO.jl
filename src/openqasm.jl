
module OpenQASM

import PikaParser as Parser
import ..@le_str, ..opt

rules = Dict(
    # terminal productions
    :eol => Parser.seq(le";", Parser.many(Parser.satisfy(isspace))),
    :id => Parser.scan() do x
        matched = match(r"^[a-z][a-zA-Z0-9_]*", x)
        isnothing(matched) && return 0
        length(matched.match)
    end,
    :real => Parser.scan(Base.Fix1(occursin, r"^([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?")),
    :nninteger => Parser.some(Parser.satisfy(isdigit)),
    :ws => Parser.some(Parser.satisfy(isspace)),
    :unaryop => Parser.first(le"sin", le"cos", le"tan", le"exp", le"ln", le"sqrt"),
    # grammar rules
    :mainprogram => Parser.seq(:header, :program),
    :header => Parser.seq(
        le"OPENQASM",
        :ws,
        :version => Parser.seq(Parser.satisfy(isdigit), le".", Parser.satisfy(isdigit)),
        :eol,
    ),
    :program => Parser.seq(Parser.many(:program), :statement),
    :statement => Parser.first(
        :decl,
        Parser.seq(:gatedecl, opt(:goplist), le"}"),
        Parser.seq(le"opaque", :id, opt(Parser.seq(le"(", opt(:idlist), le")")), :idlist, :eol),
        :qop,
        Parser.seq(le"if", le"(", :id, le"==", :nninteger, le")", :qop),
        Parser.seq(le"barrier", :anylist, :eol),
    ),
    :decl => Parser.seq(
        Parser.first(le"qreg", le"creg"),
        :ws,
        :id,
        :decl_opt_length => opt(Parser.seq(le"[", :nninteger, le"]")),
        :eol,
    ),
    :gatedecl => Parser.seq(le"gate", :id, opt(Parser.seq(le"(", opt(:idlist), le")")), :idlist, :eol),
    :goplist => Parser.first(
        :uop,
        Parser.seq(le"barrier", :idlist, :eol),
        Parser.seq(:goplist, :uop),
        Parser.seq(:goplist, le"barrier", :idlist, :eol),
    ),
    :qop => Parser.first(
        :uop,
        Parser.seq(le"measure", :argument, le"->", :argument, :eol),
        Parser.seq(le"reset", :argument, :eol),
    ),
    :uop => Parser.first(
        Parser.seq(le"U", le"(", :explist, le")", :argument, :eol),
        Parser.seq(le"CX", :argument, le",", :argument, :eol),
        Parser.seq(:id, opt(Parser.seq(le"(", opt(:explist), le")")), :anylist, :eol),
    ),
    :anylist => Parser.first(:idlist, :mixedlist),
    :idlist => Parser.first(:id, Parser.seq(:idlist, le",", :id)),
    :mixedlist => Parser.first(
        Parser.seq(:id, le"[", :nninteger, le"]"),
        Parser.seq(:mixedlist, le",", :id, opt(Parser.seq(le"[", :nninteger, le"]"))),
        Parser.seq(:idlist, le",", :id, le"[", :nninteger, le"]"),
    ),
    :argument => Parser.first(:id, Parser.seq(:id, le"[", :nninteger, le"]")),
    :explist => Parser.first(:exp, Parser.seq(:explist, le",", :exp)),
    :exp => Parser.first(
        :real,
        :nninteger,
        :pi => le"pi",
        :id,
        Parser.seq(:exp, Parser.first(le"+", le"-", le"*", le"/", le"^"), :exp),
        Parser.seq(le"-", :exp),
        Parser.seq(le"(", :exp, le")"),
        Parser.seq(:unaryop, le"(", :exp, le")"),
    ),
)

grammar = Parser.make_grammar([:mainprogram], Parser.flatten(rules, Char))

struct RegisterDeclaration
    name::String
    length::Int
    isquantum::Bool
end

folds = Dict(
    # terminal productions
    :id => (v, s) -> v,
    :real => (v, s) -> parse(Float64, v),
    :nninteger => (v, s) -> parse(Int64, v),
    :version => (v, s) -> VersionNumber(v),
    :unaryop => (v, s) -> begin
        v == "sin" && return sin
        v == "cos" && return cos
        v == "tan" && return tan
        v == "exp" && return exp
        v == "ln" && return log
        v == "sqrt" && return sqrt
    end,
    :pi => pi,
    # grammar rules
    :header => (v, s) -> s[3],
    :decl => (v, s) -> RegisterDeclaration(s[3], s[4], s[1] == "qreg"), # TODO
    :decl_opt_length => (v, s) -> isempty(s) ? 1 : s[1].args[3],
)

folder(match, p, subvals) =
    if haskey(folds, match.rule)
        folds[match.rule](match.view, subvals)
    else
        Parser.default_fold(match, p, subvals)
    end

end

function QuacIO.parse(::Format{:openqasm}, io) end
