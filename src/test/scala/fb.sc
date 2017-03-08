trait A {
}

trait B {
  self: A =>
}

val c: B  = new B with A
