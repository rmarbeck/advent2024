// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day24 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "56729630917616")
    assertEquals(score2, "bjm,hsw,nvr,skf,wkr,z07,z13,z18")

  test("Day24 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "4")
    assertEquals(score2, "N/A")
