// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day16 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "90440")
    assertEquals(score2, "479")

  test("Day16 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "7036")
    assertEquals(score2, "45")
