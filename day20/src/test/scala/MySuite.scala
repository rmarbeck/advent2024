// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day20 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1296")
    assertEquals(score2, "977665")

  test("Day20 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "8")
    assertEquals(score2, "2159")
