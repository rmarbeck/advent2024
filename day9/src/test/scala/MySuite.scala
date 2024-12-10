// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day9 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "6415184586041")
    assertEquals(score2, "6436819084274")

  test("Day9 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "1928")
    assertEquals(score2, "2858")
