package util

def solveItp(f: Double => Double, domain: (Double, Double)): Double = {
  inline val epsilon = 1e-10
  inline val n0 = 1
  var a = domain(0)
  var b = domain(1)
  var ya = f(a)
  var yb = f(b)
  if (ya > yb) {
    return solveItp(x => -f(x), domain)
  }
  val k1 = 0.2 / (b - a)
  val n12 =
    Math
      .max(
        Math.ceil(Math.log((b - a) / epsilon) / Math.log(2)) - 1.0,
        0.0
      )
      .toLong
  val nMax = n0 + n12
  var scaledEpsilon = epsilon * (1L << nMax)
  while (b - a > 2.0 * epsilon) {
    val x12 = 0.5 * (a + b);
    val r = scaledEpsilon - 0.5 * (b - a)
    val xf = (yb * a - ya * b) / (yb - ya)
    val sigma = x12 - xf
    val delta = k1 * (b - a) * (b - a)
    val xt = if (delta <= Math.abs(x12 - xf)) {
      xf + Math.copySign(delta, sigma)
    } else {
      x12
    }
    val xitp = if (Math.abs(xt - x12) <= r) {
      xt
    } else {
      x12 - Math.copySign(r, sigma)
    }

    val yitp = f(xitp)
    if (yitp > 0.0) {
      b = xitp
      yb = yitp
    } else if (yitp < 0.0) {
      a = xitp
      ya = yitp
    } else {
      return xitp
    }
    scaledEpsilon *= 0.5
  }
  0.5 * (a + b)
}
