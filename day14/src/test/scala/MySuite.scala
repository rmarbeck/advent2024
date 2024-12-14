// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day14 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "211773366")
    assertEquals(score2, "7344")

  test("Day14 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "12")
    assertEquals(score2, "0")
