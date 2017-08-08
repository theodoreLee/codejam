import org.scalatest.FunSuite

class GMatrixUnit extends FunSuite {
  test("find maximum all subarrays of size k") {
    assert(GMatrix.findMaxVector(Vector(1,2,3,4,5,6), 3) == Vector(3,4,5,6))
    assert(GMatrix.findMaxVector(Vector(1,3,2,5,4,6), 3) == Vector(3,5,5,6))
    assert(GMatrix.findMaxVector(Vector(1,3,2,5,4,1), 3) == Vector(3,5,5,5))
    assert(GMatrix.findMaxVector(Vector(6,2,4,1,3,5), 3) == Vector(6,4,4,5))
  }

  test("#sample 1") {
    assert(GMatrix.solve(Array(1), Array(1), 1, 5, 1, 1) == 3)
  }

  test("#sample 2") {
    assert(GMatrix.solve(Array(1,2), Array(3,4), 5, 11,2,1) == 19)
  }

  test("#sample 3") {
    assert(GMatrix.solve(Array(6,4,3), Array(2,1,5), 3, 109, 3, 2) == 80)
  }
}
