// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day4 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "346386")
    assertEquals(score2, "9958218")

  test("Day4 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "609043")
    assertEquals(score2, "6742839")
