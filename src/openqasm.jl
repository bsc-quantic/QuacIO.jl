
module OpenQASM

import PikaParser as Parser
import ..@le_str, ..opt

rules = Dict(
    # terminal productions
    :eol => Parser.seq(le";", Parser.many(Parser.satisfy(isspace))),
    :id_main => Parser.scan() do x
        matched = match(r"^[a-z][a-zA-Z0-9_]*", x)
        isnothing(matched) && return 0
        length(matched.match)
    end,
    :id => Parser.seq(:id_main, :wsopt), #TODO add not_followed_by or stricter lexing to prevent split ids
    :real_nums => Parser.scan() do input
        capture = match(r"^[-]?[0-9]+\.[0-9]*([eE][-+]?[0-9]+)?", input)
        isnothing(capture) ? 0 : length(capture.match)
    end,
    :real => Parser.seq(:real_nums, :wsopt),
    :digit => Parser.satisfy(isdigit),
    :digitdot => Parser.first(:digit, le"."),
    :digitdote => Parser.first(:digitdot, le"e"),
    :nninteger => Parser.seq(Parser.some(:digit), Parser.not_followed_by(:digitdote), :wsopt),
    :ws_char => Parser.satisfy(isspace),
    :ws => Parser.some(:ws_char),
    :wsopt => Parser.many(:ws_char),
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
        Parser.first(
            :regdecloptlength => Parser.seq(le"[", :nninteger, le"]"),
            Parser.epsilon,
        ),
        :eol,
    ),

    # math expressions
    Parser.precedence_cascade(n -> Symbol(:binop_prec_, n),
        (same,next) -> :expr => Parser.first(
                :additive_expr => Parser.seq(same, Parser.first(:plus => le"+", :minus => le"-"), :wsopt, next),
                next,
            ),
        (same,next) ->
            Parser.first(
                :multiplicative_expr => Parser.seq(same, Parser.first(:times => le"*", :divides => le"/"), :wsopt, next),
                next,
            ),
        (same,next) ->
            Parser.first(
                :pow_expr =>Parser.seq(next, :power => le"^", :wsopt, same),
                next,
            ),
        (same,next) ->
            Parser.first(
                :unary_expr =>Parser.seq(
                    :unary_fun_op => Parser.first(
                        le"sin",
                        le"cos",
                        le"tan",
                        le"exp",
                        le"ln",
                        le"sqrt",
                        :unary_minus => Parser.seq(le"-", Parser.not_followed_by(:digitdot)), #spoiler: parsing of unary minuses is pure hell even in such a simple language
                    ),
                    :wsopt,
                    next,
                ),
                next,
            ),
        (_, restart) ->
            Parser.first(
                :real,
                :nninteger,
                :pi => Parser.seq(le"pi", :wsopt),
                :id, #needs wsopt; better wrap all atoms to something that whitespaces them
                :paren_expr => Parser.seq(le"(", :wsopt, restart, le")", :wsopt),
            ),
    )...,

    # register reference as argument
    :argument => Parser.first(:argumentarray => Parser.seq(:id, le"[", :nninteger, le"]"), :id),
    # quantum operators
    :uop => Parser.first(
        :uopu => Parser.seq(le"U", le"(", :wsopt, :exprlist, le")", :wsopt, :argument, :eol),
        :uopcx => Parser.seq(le"CX", :ws, :argument, le",", :wsopt, :argument, :eol),
        :uopcustom => Parser.seq(:id, opt(:uopcustom_in => Parser.seq(le"(", :wsopt, opt(:exprlist), le")", :wsopt)), :anylist, :eol),
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
    :idlist => Parser.first(:idcons => Parser.seq(:id, le",", :wsopt, :idlist), :id),
    :mixedlist => Parser.first(
        Parser.seq(:id, le"[", :nninteger, le"]"),
        Parser.seq(:mixedlist, le",", :id, opt(Parser.seq(le"[", :nninteger, le"]"))),
        Parser.seq(:idlist, le",", :id, le"[", :nninteger, le"]"),
    ),
    :exprlist => Parser.first(:exprlist_in => Parser.seq(:expr, le",", :wsopt, :exprlist), :expr),
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
    :eol => (_, _) -> nothing,
    :ws => (_, _) -> nothing,
    :wsopt => (_, _) -> nothing,
    :id_main => (v, s) -> Symbol(v),
    :id => (_,(main,_)) -> main,
    :nninteger => (v, s) -> parse(Int64, v),
    :real => (v, s) -> parse(Float64, v),
    # header
    :version => (v, s) -> VersionNumber(v),
    :header => (v, (_, _, version, _)) -> Expr(:call, :format, :openqasm, version),
    # register declaration (e.g. "qreg q[12];")
    :regdecloptlength => (v, (_, i, _)) -> i,
    :regdeclkind => (v, _) -> Symbol(v),
    :regdecl => (v, (kind, _, name, length)) -> Expr(:call, kind, name, isnothing(length) ? 1 : length),
    # math expressions
    :pi => (_, _) -> pi,
    :plus => (_, _) -> :+,
    :minus => (_, _) -> :-,
    :times => (_, _) -> :*,
    :divides => (_, _) -> :/,
    :power => (_, _) -> :^,
    :additive_expr => (v, (a, op, _, b)) -> Expr(:call, op, a, b),
    :multiplicative_expr => (v, (a, op, _, b)) -> Expr(:call, op, a, b),
    :pow_expr => (v, (a, op, _, b)) -> Expr(:call, op, a, b),
    :unary_expr => (v, (op, _, a)) -> Expr(:call, op, a),
    :unary_fun_op => (v, _) -> Symbol(v),
    :unary_minus => (_, _) -> :-,
    :base_exp => (_, (x,)) -> x,
    :paren_expr => (v, (_, _, x, _, _)) -> x,
    :expr => (v, (x,)) -> x,
    :exprlist_in => (_, (e,_,_,el)) -> [e;el],
    # register references
    :argumentarray => (v, (id, _, index, _)) -> Expr(:ref, id, index),
    # quantum operators
    :uopu => (_, (_, _, _, params, _, _, arg, _)) -> Expr(:call, :U, Expr(:tuple, params...), arg),
    :uopcx => (_, (_, _, a, _, _, b, _)) -> Expr(:call, :CX, a, b),
    :uopcustom => (_, (gate, params, targets, _)) -> Expr(:call, :uop, gate, Expr(:tuple, params...), targets...),
    :idcons => (_, (a, _, _, as)) -> [a;as],
    :uopcustom_in => (_, (_,_,x,_,_)) -> x,
    :uop => (_, (e,)) -> e,
    :qopmeasure => (_, (_, src, _, dst, _)) -> Expr(:call, :measure, src, dst),
    :qopmeasure => (_, (_, target, _)) -> Expr(:call, :reset, target),
    :qop => (_, (e,)) -> e,
    # TODO gate declaration
    # program
    :program => (_, s) -> Expr(:block, s...),
    :main => (_, (header, program)) -> Expr(:call, :program, header, program),
)

folder(match, p, subvals) =
    if haskey(folds, match.rule)
        #@info "folding" match subvals # debug
        folds[match.rule](match.view, subvals)
    else
        #@info "folding default fallthrough" match subvals # debug
        if length(subvals)==1
            subvals[1]
        else
            nothing
        end
    end
end

function QuacIO.parse(::Format{:openqasm}, io) end
