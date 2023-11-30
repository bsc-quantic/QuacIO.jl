using Test
using QuacIO.OpenQASM: grammar, rules, folder
using PikaParser

@testset "Terminal productions" begin
    @testset "end-of-line" begin
        input = ";"
        expected = nothing
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :eol, 1), fold = folder) == expected
    end

    @testset "identifier" begin
        input = "asdf24"
        expected = :asdf24
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :id, 1), fold = folder) == expected
    end

    @testset "non-negative integer: $x" for x in [0, 1, 10]
        input = "$x"
        expected = x
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :nninteger, 1), fold = folder) ==
              expected
    end

    @testset "real number: $x" for x in [0.0, 1.2, 10.0, -1.5]
        input = "$x"
        expected = x
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :real, 1), fold = folder) == expected
    end

    @testset "whitespace" begin
        input = " "
        expected = nothing
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :ws, 1), fold = folder) == expected
    end
end

# Test parsing of header
@testset "Header parsing" begin
    @testset "Valid header" begin
        input = "OPENQASM 2.0;"
        expected = Expr(:format, :openqasm, v"2.0")
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :header, 1), fold = folder) == expected
    end
end

# Test parsing of register declaration
@testset "Register declaration parsing" begin
    @testset "qreg declaration" begin
        input = "qreg q[2];"
        expected = Expr(:qreg, :q, 2)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected

        input = "qreg q;"
        expected = Expr(:qreg, :q, 1)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected
    end

    @testset "creg declaration" begin
        input = "creg c[3];"
        expected = Expr(:creg, :c, 3)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected

        input = "creg c;"
        expected = Expr(:creg, :c, 1)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected
    end
end

# Test parsing of math expressions
@testset "Math expression parsing" begin
    @testset "real number" begin
        input = "3.14"
        expected = 3.14
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "integer" begin
        input = "42"
        expected = 42
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "pi" begin
        input = "pi"
        expected = pi
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "negation" begin
        input = "-3.14"
        expected = Expr(:call, :-, 3.14)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "parentheses" begin
        input = "(3.14)"
        expected = 3.14
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "binary operation" begin
        input = "3 + 2"
        expected = Expr(:call, :+, 3, 2)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "unary operation" begin
        input = "sin(3.14)"
        expected = Expr(:call, :sin, 3.14)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end
end

@testset "Argument parsing" begin
    @testset "Single argument" begin
        input = "arg"
        expected = Expr(:ref, :arg)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :argument, 1), fold = folder) ==
              expected
    end

    @testset "Array argument" begin
        input = "arg[3]"
        expected = Expr(:ref, :arg, 3)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :argument, 1), fold = folder) ==
              expected
    end
end
