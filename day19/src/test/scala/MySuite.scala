// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day19 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "311")
    assertEquals(score2, "616234236468263")

  test("Day19 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "6")
    assertEquals(score2, "16")
