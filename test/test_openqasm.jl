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

@testset "Header parsing" begin
    @testset "Valid header" begin
        input = "OPENQASM 2.0;"
        expected = Expr(:call, :format, :openqasm, v"2.0")
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :header, 1), fold = folder) == expected
    end
end

@testset "Register declaration parsing" begin
    @testset "qreg declaration" begin
        input = "qreg q[2];"
        expected = Expr(:call, :qreg, :q, 2)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected

        input = "qreg q;"
        expected = Expr(:call, :qreg, :q, 1)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected
    end

    @testset "creg declaration" begin
        input = "creg c[3];"
        expected = Expr(:call, :creg, :c, 3)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected

        input = "creg c;"
        expected = Expr(:call, :creg, :c, 1)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :regdecl, 1), fold = folder) == expected
    end
end

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

    @testset "identifier" begin
        input = "x"
        expected = :x
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "negated literal" begin
        input = "-3.14"
        expected = -3.14
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "unary negation operator" begin
        input = "- 12.34 + -a + -(a)"
        expected = :((-(12.34) + -a) + -a)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "parentheses" begin
        input = "(3.14)"
        expected = 3.14
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "binary operation: $op" for op in [:+, :-, :*, :/, :^]
        input = "3 $op 2"
        expected = Expr(:call, op, 3, 2)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "binary operation: id * id" begin
        input = "a * b"
        expected = Expr(:call, :*, :a, :b)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "binary operation: real * id" begin
        input = "8.2 * x"
        expected = Expr(:call, :*, 8.2, :x)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "unary operation: $op" for op in [:sin, :cos, :tan, :exp, :ln, :sqrt]
        input = "$op(3.14)"
        expected = Expr(:call, op, 3.14)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected
    end

    @testset "binary operator precedence" begin
        input = "a/b*x"
        expected = Expr(:call, :*, Expr(:call, :/, :a, :b), :x)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :expr, 1), fold = folder) == expected

        input = "cos(pi/8.2 * x)^2"
        expected = Expr(:call, :^, Expr(:call, :cos, Expr(:call, :*, Expr(:call, :/, pi, 8.2), :x)), 2)
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

@testset "Uop productions parsing" begin
    @testset "uopu production" begin
        input = "U(a, b, c) arg;"
        expected = Expr(:uopu, [:a, :b, :c], :arg)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :uop, 1), fold = folder) == expected
    end

    @testset "uopcx production" begin
        input = "CX arg1, arg2;"
        expected = Expr(:uopcx, :arg1, :arg2)
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :uop, 1), fold = folder) == expected
    end

    @testset "uopcustom production" begin
        input = "custom(arg1, arg2) any1, any2;"
        expected = Expr(:uopcustom, :custom, [:arg1, :arg2], [:any1, :any2])
        state = PikaParser.parse(grammar, input)
        @test PikaParser.traverse_match(state, PikaParser.find_match_at!(state, :uop, 1), fold = folder) == expected
    end
end
