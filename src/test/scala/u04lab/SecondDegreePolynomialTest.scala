package u04lab

import org.junit.*
import org.junit.Assert.*
import u04lab.code.SecondDegreePolynomial

class SecondDegreePolynomialTest:

  val simplePolynomial: SecondDegreePolynomial = SecondDegreePolynomial(1.0, 0, 3)
  val anotherPolynomial: SecondDegreePolynomial = SecondDegreePolynomial(0.0, 1, 0.0)
  val fullPolynomial: SecondDegreePolynomial = SecondDegreePolynomial(3.0, 2.0, 5.0)

  @Test def testSum() =
    val sum = simplePolynomial + anotherPolynomial
    assertEquals((sum.secondDegree, sum.firstDegree, sum.constant), (1.0, 1.0, 3.0))

  @Test def testSub() =
    val sub = simplePolynomial - anotherPolynomial
    assertEquals((sub.secondDegree, sub.firstDegree, sub.constant), (1.0, -1.0, 3.0))

  @Test def testComplex() =
    val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
    assertEquals(
      (multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant),
      (2.0, 1.0, 2.0))

  @Test def testEquals() =
    assertEquals(simplePolynomial, SecondDegreePolynomial(1.0, 0, 3))

  @Test def testToString() =
    assertEquals(simplePolynomial.toString, "SecondDegreePolynomialImpl(1.0,0.0,3.0)")

