
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
    :real => Parser.scan() do input
        capture = match(r"^([-][0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?", input)
        isnothing(capture) ? 0 : length(capture.match)
    end,
    :nninteger => Parser.some(Parser.satisfy(isdigit)),
    :ws => Parser.some(Parser.satisfy(isspace)),
    # header
    :header => Parser.seq(
        le"OPENQASM",
        :ws,
        :version => Parser.seq(Parser.satisfy(isdigit), le".", Parser.satisfy(isdigit)),
        :eol,
    ),
    # register declaration
    :regdecl => Parser.seq(
        :regdeclkind => Parser.first(le"qreg", le"creg"),
        :ws,
        :id,
        :regdecloptlength => opt(Parser.seq(le"[", :nninteger, le"]")),
        :eol,
    ),
    # math expressions
    :expr => Parser.first(
        # TODO :id,
        :expr_neg => Parser.seq(le"-", :expr),
        :expr_par => Parser.seq(le"(", :expr, le")"),
        :expr_binaryop =>
            Parser.seq(:expr, :ws, :binaryop => Parser.first(le"+", le"-", le"*", le"/", le"^"), :ws, :expr),
        :expr_unaryop => Parser.seq(
            :unaryop => Parser.first(le"sin", le"cos", le"tan", le"exp", le"ln", le"sqrt"),
            le"(",
            :expr,
            le")",
        ),
        :real,
        :nninteger,
        :pi => le"pi",
    ),
    # register reference as argument
    :argument => Parser.first(:id, :argumentarray => Parser.seq(:id, le"[", :nninteger, le"]")),
    # quantum operators
    :uop => Parser.first(
        :uopu => Parser.seq(le"U", le"(", :exprlist, le")", :argument, :eol),
        :uopcx => Parser.seq(le"CX", :ws, :argument, le",", :argument, :eol),
        :uopcustom => Parser.seq(:id, opt(Parser.seq(le"(", opt(:exprlist), le")")), :anylist, :eol),
    ),
    :qop => Parser.first(
        :uop,
        :qopmeasure => Parser.seq(le"measure", :argument, le"->", :argument, :eol),
        :qopreset => Parser.seq(le"reset", :argument, :eol),
    ),
    # gate declaration
    :gatedecl => Parser.seq(le"gate", :id, opt(Parser.seq(le"(", opt(:idlist), le")")), :idlist, :eol),
    :goplist => Parser.first(
        :uop,
        Parser.seq(le"barrier", :idlist, :eol),
        Parser.seq(:goplist, :uop),
        Parser.seq(:goplist, le"barrier", :idlist, :eol),
    ),
    # list
    :anylist => Parser.first(:idlist, :mixedlist),
    :idlist => Parser.first(:id, Parser.seq(:idlist, le",", :id)),
    :mixedlist => Parser.first(
        Parser.seq(:id, le"[", :nninteger, le"]"),
        Parser.seq(:mixedlist, le",", :id, opt(Parser.seq(le"[", :nninteger, le"]"))),
        Parser.seq(:idlist, le",", :id, le"[", :nninteger, le"]"),
    ),
    :exprlist => Parser.first(:expr, Parser.seq(:exprlist, le",", :expr)),
    # program
    :statement => Parser.first(
        :regdecl,
        Parser.seq(:gatedecl, opt(:goplist), le"}"),
        # TODO Parser.seq(le"opaque", :id, opt(Parser.seq(le"(", opt(:idlist), le")")), :idlist, :eol),
        :qop,
        # TODO Parser.seq(le"if", le"(", :id, le"==", :nninteger, le")", :qop),
        # TODO Parser.seq(le"barrier", :anylist, :eol),
    ),
    :program => Parser.some(:statement),
    :main => Parser.seq(:header, :program),
)

grammar = Parser.make_grammar([:main], Parser.flatten(rules, Char))

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
