object Solver {
  def solve(N: Int, S: String): String = {
    val isOdd = N % 2 == 1

    var THead = S(if (isOdd) N - 2 else N - 1)

    S.reverse
      .substring(if (isOdd) 1 else 0)
      .zipWithIndex
      .filter { case (char, index) =>
        val filterCondition =
          if (index % 2 == 0) char >= THead else char < THead
        if (filterCondition) THead = char
        filterCondition
      }
      .map(_._1)
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
