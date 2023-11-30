using Test
using QuacIO

@testset "File formats" verbose = true begin
    @testset "OpenQASM" verbose = true begin
        include("test_openqasm.jl")
    end
end