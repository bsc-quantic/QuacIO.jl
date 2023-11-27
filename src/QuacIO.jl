module QuacIO

using Quac

struct Format{X} end

macro Format_str(x)
    return :(Format{x}())
end

function parse end

function QuacIO.parse(filename; format, kwargs...)
    io = open(filename, "r")

    try
        return QuacIO.parse(format, io; kwargs...)
    finally
        close(io)
    end
end

include("qflex.jl")

export parse

end
