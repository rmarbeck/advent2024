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



    (1 to 276655).map(aValue => s" $aValue\t${aValue.toBinaryString} => ${runDevice(prog, Registers(aValue, 0, 0), prog)}").filter(_.contains(" => 2,4,1,2,7,5")).foreach(println)

    val result2 = 0//test(prog, 1, toFind)

    (s"$result1", s"$result2")

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

def search(toFind: List[Int], current: Int, factor: Int = 1, found: List[Int], program: List[(Instruction, Operand)]): List[Int] =
  runDevice(program, Registers(current, 0, 0), program) match
    case output if output == toFind.take(found.length + 1).mkString(",") => search(toFind, current + factor, 8, current :: found, program)
    case _ => search(toFind, current + factor, factor, current :: found, program)


@tailrec
def test(program: List[(Instruction, Operand)], aValue: Int, toFind: List[Int]): Int =
  runDevice2(program, Registers(aValue, 0, 0), program, toFind) match
    case true => aValue
    case false => test(program, aValue + 1, toFind)

def canMatch(output: List[Int], toFind: List[Int]): (Boolean, Boolean) =
  if (output.reverse == toFind)
    (true, true)
  else if (toFind.startsWith(output.reverse))
    (true, false)
  else
    (false, false)


@tailrec
def runDevice2(program: List[(Instruction, Operand)], registers: Registers, fullProgram: List[(Instruction, Operand)], toFind: List[Int], output: List[Int] = Nil): Boolean =
  canMatch(output, toFind) match
    case (false, _) => false
    case (true, true) => true
    case _ =>
      given Registers = registers
      program match
        case Nil => canMatch(output, toFind)._2
        case (currentInst, currentOperand) :: tail =>
          instructions.fromOrdinal(currentInst.opcode) match
            case ADV =>
              runDevice2(tail, registers.setA((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, toFind, output)
            case BXL =>
              runDevice2(tail, registers.setB(registers.b ^ currentOperand.operandCode), fullProgram, toFind, output)
            case BST =>
              runDevice2(tail, registers.setB(currentOperand.get % 8), fullProgram, toFind, output)
            case JNZ =>
              registers.a match
                case 0 => runDevice2(tail, registers, fullProgram, toFind, output)
                case other =>
                  runDevice2(fullProgram.drop(currentOperand.operandCode / 2), registers, fullProgram, toFind, output)
            case BXC =>
              runDevice2(tail, registers.setB(registers.b ^ registers.c), fullProgram, toFind, output)
            case OUT =>
              runDevice2(tail, registers, fullProgram, toFind, currentOperand.get % 8 :: output)
            case BDV =>
              runDevice2(tail, registers.setB((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, toFind, output)
            case CDV =>
              runDevice2(tail, registers.setC((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, toFind, output)

@tailrec
def runDevice(program: List[(Instruction, Operand)], registers: Registers, fullProgram: List[(Instruction, Operand)], output: List[Int] = Nil): String =
  given Registers = registers
  program match
    case Nil => output.reverse.mkString(",")
    case (currentInst, currentOperand) :: tail =>
      instructions.fromOrdinal(currentInst.opcode) match
        case ADV =>
          runDevice(tail, registers.setA((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, output)
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
          runDevice(tail, registers, fullProgram, currentOperand.get % 8 :: output)
        case BDV =>
          runDevice(tail, registers.setB((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, output)
        case CDV =>
          runDevice(tail, registers.setC((registers.a / Math.pow(2, currentOperand.get)).toInt), fullProgram, output)

case class Instruction(opcode: Int)

case class Operand(operandCode: Int):
  def get(using registers: Registers): Int =
    operandCode match
      case 0 | 1 | 2 | 3 => operandCode
      case 4 => registers.a
      case 5 => registers.b
      case 6 => registers.c
      case 7 => throw Exception("Reserved")

case class Registers(a: Int, b: Int, c: Int):
  def setA(newValue: Int): Registers = this.copy(a = newValue)
  def setB(newValue: Int): Registers = this.copy(b = newValue)
  def setC(newValue: Int): Registers = this.copy(c = newValue)
