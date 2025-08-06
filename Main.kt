import java.io.FileInputStream
import java.io.InputStream
import java.io.OutputStream
import java.util.Stack

enum class Token {
    Right,
    Left,
    Increment,
    Decrement,
    Output,
    Accept,
    JumpIfZero,
    JumpIfNonZero,
}

fun Char.toToken() =
    when (this) {
        '>' -> Token.Right
        '<' -> Token.Left
        '+' -> Token.Increment
        '-' -> Token.Decrement
        '.' -> Token.Output
        ',' -> Token.Accept
        '[' -> Token.JumpIfZero
        ']' -> Token.JumpIfNonZero
        else -> null
    }

fun Token.toChar() =
    when (this) {
        Token.Right -> '>'
        Token.Left -> '<'
        Token.Increment -> '+'
        Token.Decrement -> '-'
        Token.Output -> '.'
        Token.Accept -> ','
        Token.JumpIfZero -> '['
        Token.JumpIfNonZero -> ']'
    }

class Instruction(
    val command: Token,
    var argument: Int,
)

class Compiler(
    val code: String,
) {
    val codeLength = code.length

    private var position: Int
    private val instructions: MutableList<Instruction>

    init {
        position = 0
        instructions = mutableListOf()
    }

    fun compile(): List<Instruction> {
        position = 0
        instructions.clear()

        val loopStack = Stack<Int>()

        while (position < codeLength) {
            val character = code[position]

            val token = character.toToken()
            when (token) {
                Token.JumpIfZero -> {
                    val instruction = Instruction(token, 0)
                    val instructionPosition = emitWithArg(instruction)
                    loopStack.push(instructionPosition)
                }
                Token.JumpIfNonZero -> {
                    val openInstruction = loopStack.pop()
                    val instruction = Instruction(token, openInstruction)
                    val closeInstructionPosition = emitWithArg(instruction)
                    instructions[openInstruction].argument = closeInstructionPosition
                }
                Token.Right, Token.Left, Token.Increment, Token.Decrement,
                Token.Output, Token.Accept,
                -> compileFoldableInstruction(token)
                else -> {}
            }

            position++
        }

        return instructions
    }

    fun compileFoldableInstruction(token: Token) {
        var count = 1
        while (position < codeLength - 1 && code[position + 1] == token.toChar()) {
            count++
            position++
        }

        emitWithArg(Instruction(token, count))
    }

    private fun emitWithArg(instruction: Instruction): Int {
        instructions += instruction
        return instructions.size - 1
    }
}

class VM(
    val instructions: List<Instruction>,
    val input: InputStream,
    val output: OutputStream,
) {
    private var ip: Int
    private var dp: Int
    private val memory: ShortArray
    private val buf: ByteArray

    init {
        ip = 0
        dp = 0
        memory = ShortArray(30_000)
        buf = ByteArray(1)
    }

    fun execute() {
        while (ip < instructions.size) {
            val ins = instructions[ip]

            when (ins.command) {
                Token.Increment -> {
                    (1..ins.argument).forEach { _ ->
                        memory[dp]++
                        if (memory[dp] == 256.toShort()) {
                            memory[dp] = 0.toShort()
                        }
                    }
                }
                Token.Decrement -> {
                    (1..ins.argument).forEach { _ ->
                        memory[dp]--
                        if (memory[dp] == (-1).toShort()) {
                            memory[dp] = 255.toShort()
                        }
                    }
                }
                Token.Right -> dp += ins.argument
                Token.Left -> dp -= ins.argument
                Token.Accept -> {
                    (1..ins.argument).forEach { _ ->
                        readChar()
                    }
                }
                Token.Output -> {
                    (1..ins.argument).forEach { _ ->
                        putChar()
                    }
                }
                Token.JumpIfZero -> {
                    if (memory[dp] == 0.toShort()) {
                        ip = ins.argument
                        continue
                    }
                }
                Token.JumpIfNonZero -> {
                    if (memory[dp] != 0.toShort()) {
                        ip = ins.argument
                        continue
                    }
                }
            }

            ip++
        }
    }

    private fun readChar() {
        val res = input.read(buf)

        check(res != -1) { "Wrong num bytes read" }

        memory[dp] = buf[0].toShort()
    }

    private fun putChar() {
        buf[0] = memory[dp].toByte()

        output.write(buf)
        output.flush()
    }
}

fun main(vararg args: String) {
    if (args.size != 1) {
        println("No fileName given")
        return
    }
    val fileName = args[0]
    val inputStream = FileInputStream(fileName)
    val code = String(inputStream.readAllBytes())

    val compiler = Compiler(code)
    val instructions = compiler.compile()

    val machine = VM(instructions, System.`in`, System.out)
    machine.execute()
}
