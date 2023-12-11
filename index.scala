object solver {
  private def solver(N: Int, S: String): String = {
    val isOdd = if (N % 2 == 0) false else true
    var head: Char = S.apply(if (isOdd) N - 2 else N - 1)
    var index = 0
    S.reverse
      .substring(if (isOdd) 1 else 0)
      .filter(p => {
        val filter: Boolean =
          if (index % 2 == 0) p.compareTo(head) >= 0
          else p.compareTo(head) < 0
        if (filter) head = p
        index += 1
        filter
      })
      .reverse
  }
  def main(args: Array[String]): Unit = {
    assert(solver(2, "aa") == "a", "test1 failed")
    assert(solver(4, "abcd") == "acd", "test2 failed")
    assert(solver(10, "aaaaaaaabb") == "aaaab", "test3 failed")
    assert(solver(6, "xskszy") == "sky", "test4 failed")
    println("✅ All tests succeeded")
  }
}
