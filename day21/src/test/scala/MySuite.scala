// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day21 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "202648")
    assertEquals(score2, "248919739734728")

  test("Day21 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "126384")
    assertEquals(score2, "154115708116294")
