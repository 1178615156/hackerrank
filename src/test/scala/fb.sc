def f(x: Float): Float = {
  def impl(n: Int): Float =
    if (n == 10)
      1
    else
      1 + x / n * impl(n + 1)

  impl(1)
  // Compute and Return the value of e^x
}
f(1)