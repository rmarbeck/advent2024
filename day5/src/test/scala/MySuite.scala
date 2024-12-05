// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day5 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "6242")
    assertEquals(score2, "5169")

  test("Day5 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "143")
    assertEquals(score2, "123")
