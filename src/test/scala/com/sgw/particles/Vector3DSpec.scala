package com.sgw.particles

import com.sgw.particles.model.Vector3D
import org.scalatest.{FunSuite, Matchers}


class Vector3DSpec extends FunSuite with Matchers {
  test("The Vector3D subtract method should return the difference vector between this vector and another vector") {
    val v1 = Vector3D(5, 5, 5)
    val v2 = Vector3D(6, 6, 6)
    val diff = v1.sub(v2)
    diff.x should be (-1)
    diff.y should be (-1)
    diff.z should be (-1)
  }

  test("The Vector3D normalize method should return a normalized vector.") {
    val v1 = Vector3D(1.0, 0.0, 0.0).normalize
    v1.x should be (1.0 +- 0.00001)
    v1.y should be (0.0)
    v1.z should be (0.0)

    val v2 = Vector3D(1.0, 1.0, 1.0).normalize
    v2.x should be (0.57735 +- 0.00001)
    v2.y should be (0.57735 +- 0.00001)
    v2.z should be (0.57735 +- 0.00001)
  }

  test("The Vector3D projectOnTo method should return the projection of one vector onto another.") {
    val v1 = Vector3D(1.0, 0.0, 0.0).projectOnTo(Vector3D(1.0, 1.0, 1.0))
    v1.x should be (0.33333 +- 0.00001)
    v1.y should be (0.33333 +- 0.00001)
    v1.z should be (0.33333 +- 0.00001)

    val v2 = Vector3D(1.0, 0.0, 0.0).projectOnTo(Vector3D(-1.0, 1.0, 1.0))
    v2.x should be (0.33333 +- 0.00001)
    v2.y should be (-0.33333 +- 0.00001)
    v2.z should be (-0.33333 +- 0.00001)

    val v3 = Vector3D(-1.0, 0.0, 0.0).projectOnTo(Vector3D(1.0, 1.0, 1.0))
    v3.x should be (-0.33333 +- 0.00001)
    v3.y should be (-0.33333 +- 0.00001)
    v3.z should be (-0.33333 +- 0.00001)

    val v4 = Vector3D(-1.0, 1.0, 0.0).projectOnTo(Vector3D(2.0, 1.0, 0.0))
    v4.x should be (-0.39999 +- 0.00001)
    v4.y should be (-0.19999 +- 0.00001)
    v4.z should be (-0.00000 +- 0.00001)
  }

  test("Rounding a Vector3D should round every element of the vector.") {
    val v1 = Vector3D(1.0, 1.5, 10.99)

    val v1Rounded = v1.round

    v1Rounded should be (Vector3D(1.0, 2.0, 11.0))
  }

  test("Rounding to a decimal place should round every element of the vector.") {
    val v1 = Vector3D(1.1234, 23.425, 100000.24925)

    val v1Rounded = v1.roundTo(2)

    v1Rounded should be (Vector3D(1.12, 23.43, 100000.25))
  }
}
