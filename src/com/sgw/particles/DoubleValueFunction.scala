package com.sgw.particles

trait DoubleValueFunction {
  def apply(x: Double): Double
}

case class ConstantFunc(const: Double) extends DoubleValueFunction {
  def apply(x: Double) = const
}

case class RandomValueFunction(minFunc: DoubleValueFunction, maxFunc: DoubleValueFunction) extends DoubleValueFunction {
  def apply(x: Double) = Math.random() * (maxFunc(x) - minFunc(x)) + minFunc(x)
}
