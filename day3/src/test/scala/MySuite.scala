// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day3 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "178538786")
    assertEquals(score2, "102467299")

  test("Day3 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "161")
    assertEquals(score2, "48")
