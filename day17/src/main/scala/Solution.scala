import scala.annotation.tailrec

object Solution:
  def run(inputLines: Seq[String]): (String, String) =

    val (registersRaw, program) = inputLines.span(_.startsWith("Register"))

    val Seq(a, b, c) =
      registersRaw.collect:
        case s"Register $_: $value" => value.toInt

    val registers = Registers(a, b, c)

    val prog = program.last match
        case s"Program: $values" => values.split(",").map(_.toInt).grouped(2).toList.collect:
          case Array(first, second) => (Instruction(first), Operand(second))

    val result1 = runDevice(prog, registers, prog)
    val toFind =
      prog.flatMap:
        case (inst, op) => List(inst.opcode, op.operandCode)

    val (result2, _) = toFind.reverse.foldLeft((0L, Nil): (Long, List[Int])):
      case (acc, digit) =>
        ((acc._1 to acc._1 + 64).find(a => runDevice(prog, Registers(a, 0, 0), prog).split(",").map(_.toInt).toList == (digit :: acc._2)).get * 8, digit :: acc._2)

    (s"$result1", s"${result2 / 8}")

enum instructions:
  case ADV
  case BXL
  case BST
  case JNZ
  case BXC
  case OUT
  case BDV
  case CDV

import instructions._

@tailrec
def runDevice(program: List[(Instruction, Operand)], registers: Registers, fullProgram: List[(Instruction, Operand)], output: List[Int] = Nil): String =
  def cutA(currentOperand: Operand): Long = (registers.a / Math.pow(2, currentOperand.get.toInt)).toLong
  given Registers = registers
  program match
    case Nil => output.reverse.mkString(",")
    case (currentInst, currentOperand) :: tail =>
      instructions.fromOrdinal(currentInst.opcode) match
        case ADV =>
          runDevice(tail, registers.setA(cutA(currentOperand)), fullProgram, output)
        case BXL =>
          runDevice(tail, registers.setB(registers.b ^ currentOperand.operandCode), fullProgram, output)
        case BST =>
          runDevice(tail, registers.setB(currentOperand.get % 8), fullProgram, output)
        case JNZ =>
          registers.a match
            case 0 => runDevice(tail, registers, fullProgram, output)
            case other =>
              runDevice(fullProgram.drop(currentOperand.operandCode / 2), registers, fullProgram, output)
        case BXC =>
          runDevice(tail, registers.setB(registers.b ^ registers.c), fullProgram, output)
        case OUT =>
          runDevice(tail, registers, fullProgram, (currentOperand.get % 8).toInt :: output)
        case BDV =>
          runDevice(tail, registers.setB(cutA(currentOperand)), fullProgram, output)
        case CDV =>
          runDevice(tail, registers.setC(cutA(currentOperand)), fullProgram, output)

case class Instruction(opcode: Int)

case class Operand(operandCode: Int):
  def get(using registers: Registers): Long =
    operandCode match
      case 0 | 1 | 2 | 3 => operandCode
      case 4 => registers.a
      case 5 => registers.b
      case 6 => registers.c
      case 7 => throw Exception("Reserved")

case class Registers(a: Long, b: Long, c: Long):
  def setA(newValue: Long): Registers = this.copy(a = newValue)
  def setB(newValue: Long): Registers = this.copy(b = newValue)
  def setC(newValue: Long): Registers = this.copy(c = newValue)
