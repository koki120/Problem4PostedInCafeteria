object Solver {
  def solve(N: Int, S: String): String = {
    val isOdd = N % 2 == 1
    var head = S(if (isOdd) N - 2 else N - 1)
    var index = 0

    S.reverse
      .substring(if (isOdd) 1 else 0)
      .filter { p =>
        val filter = if (index % 2 == 0) p >= head else p < head
        if (filter) head = p
        index += 1
        filter
      }
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
