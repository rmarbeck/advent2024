// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day23 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1327")
    assertEquals(score2, "df,kg,la,mp,pb,qh,sk,th,vn,ww,xp,yp,zk")

  test("Day23 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "7")
    assertEquals(score2, "co,de,ka,ta")
