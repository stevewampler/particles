package com.sgw.particles

/**
 * Created with IntelliJ IDEA.
 * User: swampler
 * Date: 4/4/13
 * Time: 8:03 PM
 * Copyright 2013: Steve Wampler
 */

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class Vector3DSpec extends FlatSpec with ShouldMatchers {
  "The Vector3D subtract method" should "return the difference vector between this vector and another vector" in {
    val v1 = Vector3D(5, 5, 5)
    val v2 = Vector3D(6, 6, 6)
    val diff = v1.sub(v2)
    diff.x should be (-1)
    diff.y should be (-1)
    diff.z should be (-1)
  }

  "The Vector3D normalize method" should "return a normalized vector." in {
    val v1 = Vector3D(1.0, 0.0, 0.0).normalize
    v1.x should be (1.0 plusOrMinus 0.00001)
    v1.y should be (0.0)
    v1.z should be (0.0)

    val v2 = Vector3D(1.0, 1.0, 1.0).normalize
    v2.x should be (0.57735 plusOrMinus 0.00001)
    v2.y should be (0.57735 plusOrMinus 0.00001)
    v2.z should be (0.57735 plusOrMinus 0.00001)
  }

  "The Vector3D projectOnTo method" should "return the projection of one vector onto another." in {
    val v1 = Vector3D(1.0, 0.0, 0.0).projectOnTo(Vector3D(1.0, 1.0, 1.0))
    v1.x should be (0.33333 plusOrMinus 0.00001)
    v1.y should be (0.33333 plusOrMinus 0.00001)
    v1.z should be (0.33333 plusOrMinus 0.00001)

    val v2 = Vector3D(1.0, 0.0, 0.0).projectOnTo(Vector3D(-1.0, 1.0, 1.0))
    v2.x should be (0.33333 plusOrMinus 0.00001)
    v2.y should be (-0.33333 plusOrMinus 0.00001)
    v2.z should be (-0.33333 plusOrMinus 0.00001)

    val v3 = Vector3D(-1.0, 0.0, 0.0).projectOnTo(Vector3D(1.0, 1.0, 1.0))
    v3.x should be (-0.33333 plusOrMinus 0.00001)
    v3.y should be (-0.33333 plusOrMinus 0.00001)
    v3.z should be (-0.33333 plusOrMinus 0.00001)
  }
}
