// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day17 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "4,3,7,1,5,3,0,5,4")
    assertEquals(score2, "190384615275535")

  test("Day17 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "5,7,3,0")
    assertEquals(score2, "117440")
