// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day13 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "709")
    assertEquals(score2, "668")

  test("Day13 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "330")
    assertEquals(score2, "286")
