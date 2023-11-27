import PikaParser as Parser

const ϵ = Parser.epsilon

# helper macro for lexemes
macro le_str(x)
    return length(x) == 1 ? :(Parser.token($(only(x)))) : :(Parser.tokens($x))
end

opt(x) = Parser.first(x, ϵ)
