// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite:
  test("Day2 : main test on final data"):
    val (score1, score2) = Solver.solve
    assertEquals(score1, "1548965")
    assertEquals(score2, "3842356")

  test("Day2 : main test on initial data"):
    val (score1, score2) = Solver.solveTest
    assertEquals(score1, "58")
    assertEquals(score2, "34")
