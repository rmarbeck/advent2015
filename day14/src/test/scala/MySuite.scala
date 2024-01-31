// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day14 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "2655")
    assertEquals(score2, "1059")

  test("Day14 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "2660")
    assertEquals(score2, "1564")
