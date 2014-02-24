package com.sgw.particles

trait Updatable {
  def beginUpdate(t: Double) = {}
  def update(t: Double)
  def endUpdate(t: Double) = {}
}
