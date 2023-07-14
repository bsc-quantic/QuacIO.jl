module QuacIO

using Quac

abstract type Format end

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
