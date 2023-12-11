object Solver {
  def solve(N: Int, S: String): String = {
    val isOdd = N % 2 == 1

    var THead = S(if (isOdd) N - 2 else N - 1)

    S.reverse
      .substring(if (isOdd) 1 else 0)
      .zipWithIndex
      .filter { case (c, i) =>
        val filterCondition = if (i % 2 == 0) c >= THead else c < THead
        if (filterCondition) THead = c
        filterCondition
      }
      .map { case (c, _) => c }
      .mkString
      .reverse
  }

  def main(args: Array[String]): Unit = {
    assert(solve(2, "aa") == "a", "test1 failed")
    assert(solve(4, "abcd") == "acd", "test2 failed")
    assert(solve(10, "aaaaaaaabb") == "aaaab", "test3 failed")
    assert(solve(6, "xskszy") == "sky", "test4 failed")
    println("✅ All tests succeeded")
  }
}
