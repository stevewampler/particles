/*
Copyright 2012 Twitter, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package twitter.scalding

import java.util.concurrent.TimeUnit
import java.io.File
import java.net.URI

/**
 * The args class does a simple command line parsing.  The rules are:
 * keys start with one or more "-". Each key has zero or more values
 * following.
 */
object Args {
  /**
   * Split on whitespace and then parse.
   */
  def apply(argString : String) : Args = Args(argString.split("\\s+"))

  /**
   * parses keys as starting with a dash, except single dashed digits.
   * All following non-dashed args are a list of values.
   * If the list starts with non-dashed args, these are associated with the
   * empty string: ""
   **/
  def apply(args : Iterable[String]) : Args = {
    // def startingDashes(word : String) = word.takeWhile { _ == '-' }.length
    new Args(
      //Fold into a list of (arg -> List[values])
      args
        .filter{ a => !a.matches("\\s*") }
        .foldLeft(List[Pair[String, List[String]]]()) { (acc, arg) =>
        val noDashes = arg.dropWhile{ _ == '-'}
        if(arg == noDashes || isNumber(arg))
          (acc.head._1 -> (arg :: acc.head._2)) :: acc.tail
        else
          (noDashes -> List()) :: acc
      }
        //Now reverse the values to keep the same order
        .map {case (key, value) => key -> value.reverse}.toMap
    )
  }

  def isNumber(arg : String) : Boolean = {
    try {
      arg.toDouble
      true
    }
    catch {
      case e : NumberFormatException => false
    }
  }
}

class Args(val m : Map[String,List[String]]) extends java.io.Serializable {

  //Replace or add a given key+args pair:
  def +(keyvals : (String,Iterable[String])) : Args  = new Args(m + (keyvals._1 -> keyvals._2.toList))

  /**
   * Does this Args contain a given key?
   */
  def boolean(key : String) : Boolean = m.contains(key)

  /**
   * Get the list of values associated with a given key.
   * if the key is absent, return the empty list.  NOTE: empty
   * does not mean the key is absent, it could be a key without
   * a value.  Use boolean() to check existence.
   */
  def list(key : String) : List[String] = m.get(key).getOrElse(List())

  /**
   * This is a synonym for required
   */
  def apply(key : String) : String = required(key)

  /**
   * Gets the list of positional arguments
   */
  def positional : List[String] = list("")

  /**
   * return required positional value.
   */
  def required(position: Int) : String = positional match {
    case l if l.size > position => l(position)
    case _ => sys.error("Please provide " + (position + 1) + " positional arguments")
  }

  /**
   * This is a synonym for required
   */
  def apply(position : Int) : String = required(position)

  override def equals(other : Any) : Boolean = {
    other match {
      case args: Args =>
        args.m.equals(m)
      case _ =>
        false
    }
  }

  def get(key : String): Option[String]      = optional(key)
  def getLong(key: String): Option[Long]     = get(key).map(_.toLong)
  def getInt(key: String):  Option[Int]      = get(key).map(_.toInt)
  def getString(key: String): Option[String] = get(key)
  def getStrings(key: String): Option[List[String]] = get(key).map(_.split(",").toList)
  def getStringSet(key: String): Option[Set[String]] = getStrings(key).map(_.toSet)
  def getTimeUnit(key: String): Option[TimeUnit] = get(key).map(TimeUnit.valueOf)
  def getFile(key: String): Option[File]     = get(key).map(new File(_))
  def getURI(key: String): Option[URI]       = get(key).map(new URI(_))

  /**
   * return exactly one value for a given key.
   * If there is more than one value, you get an exception
   */
  def required(key : String) : String = list(key) match {
    case List() => sys.error("Please provide a value for --" + key)
    case List(a) => a
    case _ => sys.error("Please only provide a single value for --" + key)
  }

  def toList : List[String] = {
    m.foldLeft(List[String]()) { (args, kvlist) =>
      val k = kvlist._1
      val values = kvlist._2
      if( k != "") {
        //Make sure positional args are first
        (("--" + k) :: values) ++ args
      }
      else {
        // These are positional args (no key), put them first:
        args ++ values
      }
    }
  }

  // TODO: if there are spaces in the keys or values, this will not round-trip
  override def toString : String = toList.mkString(" ")

  /**
   * If there is zero or one element, return it as an Option.
   * If there is a list of more than one item, you get an error
   */
  def optional(key : String) : Option[String] = list(key) match {
    case List() => None
    case List(a) => Some(a)
    case _ => sys.error("Please provide at most one value for --" + key)
  }
}