// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day11 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "185894")
    assertEquals(score2, "221632504974231")

  test("Day11 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "55312")
    assertEquals(score2, "65601038650482")
