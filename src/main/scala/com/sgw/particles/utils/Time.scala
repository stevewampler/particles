package com.sgw.particles.utils

import java.util.concurrent.TimeUnit
import java.util.Date

/**
 * A factory for Time objects.
 */
object Time {
  lazy val ZERO     = millis(0L)
  lazy val INFINITE = millis(Long.MaxValue)
  lazy val INVALID  = INFINITE

  val MILLIS_PER_SECOND = 1000
  val MILLIS_PER_MINUTE = MILLIS_PER_SECOND * 60
  val MILLIS_PER_HOUR   = MILLIS_PER_MINUTE * 60
  val MILLIS_PER_DAY    = MILLIS_PER_HOUR * 24

  /**
   * Functions used to create Time object with various TimeUnits.
   * @param value the tiem value
   * @return a Time object
   */
  def nanos(value: Long)   = Time(value, TimeUnit.NANOSECONDS)
  def micros(value: Long)  = Time(value, TimeUnit.MICROSECONDS)
  def millis(value: Long)  = Time(value, TimeUnit.MILLISECONDS)
  def seconds(value: Long) = Time(value, TimeUnit.SECONDS)
  def minutes(value: Long) = Time(value, TimeUnit.MINUTES)
  def hours(value: Long)   = Time(value, TimeUnit.HOURS)
  def days(value: Long)    = Time(value, TimeUnit.DAYS)

  /**
   * Returns the current time as a Time object.
   *
   * @return the current time as a Time object.
   */
  def now = millis(System.currentTimeMillis())

  /**
   * Returns a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   *
   * @param fromTime the minimum time of the random time (inclusive)
   * @param toTime the maximum time of the random time (exclusive)
   *
   * @return a random time object between the specified fromTime (inclusive) and toTime (exclusive).
   */
  def random(fromTime: Time, toTime: Time): Time = fromTime + (toTime - fromTime).random
}

/**
 * An object representing a point in time.
 *
 * @param time the time value
 * @param units the time units
 */
case class Time(time: Long, units: TimeUnit = TimeUnit.MILLISECONDS) extends Comparable[Time] {
  def isNanos   = units == TimeUnit.NANOSECONDS
  def isMicros  = units == TimeUnit.MICROSECONDS
  def isMillis  = units == TimeUnit.MILLISECONDS
  def isSeconds = units == TimeUnit.SECONDS
  def isMinutes = units == TimeUnit.MINUTES
  def isHours   = units == TimeUnit.HOURS
  def isDays    = units == TimeUnit.DAYS

  def toNanos   = convert(TimeUnit.NANOSECONDS)
  def toMicros  = convert(TimeUnit.MICROSECONDS)
  def toMillis  = convert(TimeUnit.MILLISECONDS)
  def toSeconds = convert(TimeUnit.SECONDS)
  def toMinutes = convert(TimeUnit.MINUTES)
  def toHours   = convert(TimeUnit.HOURS)
  def toDays    = convert(TimeUnit.DAYS)
  def toDate    = new Date(millis)

  def nanos   = toNanos.time
  def micros  = toMicros.time
  def millis  = toMillis.time
  def seconds = toSeconds.time
  def minutes = toMinutes.time
  def hours   = toHours.time
  def days    = toDays.time

  def random: Time = this * scala.math.random

  def +(rhs: Time) = Time.millis(millis + rhs.millis)
  def -(rhs: Time) = Time.millis(millis - rhs.millis)
  def /(rhs: Time) = Time.millis(millis / rhs.millis)
  def *(rhs: Time) = Time.millis(millis * rhs.millis)

  def /(value: Double) = Time.millis((millis / value).toLong)
  def *(value: Double) = Time.millis((millis * value).toLong)

  def +(value: Long) = Time.millis(millis + value)
  def -(value: Long) = Time.millis(millis - value)
  def /(value: Long) = Time.millis(millis / value)
  def *(value: Long) = Time.millis(millis * value)

  def isZero     = time == 0
  def isNotZero  = !isZero
  def isPositive = time > 0
  def isPositiveOrZero = time >= 0
  def isNegative = time < 0
  def isNegativeOrZero = time <= 0
  def isInfinite = time == Long.MaxValue
  def isNotInfinite = !isInfinite

  def ==(ms: Long) = millis == ms
  def < (ms: Long) = millis <  ms
  def > (ms: Long) = millis >  ms
  def <=(ms: Long) = millis <= ms
  def >=(ms: Long) = millis >= ms

  def ==(rhs: Time) = millis == rhs.millis
  def < (rhs: Time) = millis <  rhs.millis
  def > (rhs: Time) = millis >  rhs.millis
  def <=(rhs: Time) = millis <= rhs.millis
  def >=(rhs: Time) = millis >= rhs.millis

  def compareTo(rhs: Time) = millis.compareTo(rhs.millis)

  def min(rhs: Time): Time = if (units == rhs.units) {
    if (time < rhs.time) {
      this
    } else {
      rhs
    }
  } else {
    min(rhs.convert(units))
  }

  def max(rhs: Time): Time = if (units == rhs.units) {
    if (time > rhs.time) {
      this
    } else {
      rhs
    }
  } else {
    max(rhs.convert(units))
  }

  def simplifyUnits: Time = {
    val ms = millis
    if (ms % Time.MILLIS_PER_DAY    == 0) return toDays
    if (ms % Time.MILLIS_PER_HOUR   == 0) return toHours
    if (ms % Time.MILLIS_PER_MINUTE == 0) return toMinutes
    if (ms % Time.MILLIS_PER_SECOND == 0) return toSeconds
    toMillis
  }

  def sleep() = if (isPositive) Thread.sleep(millis)

  def convert(toUnit: TimeUnit) = if (units == toUnit) this else Time(toUnit.convert(time, units), toUnit)

  def toMap = Map("time" -> time, "units" -> units)

  override def toString = time.toString + " " + units.toString.toLowerCase
}
