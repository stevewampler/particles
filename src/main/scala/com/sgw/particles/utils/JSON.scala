package com.sgw.particles.utils

import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success, Try}
import java.net.URI
import com.sgw.particles._
import com.sgw.particles.factories.{Vector3DFunctionFactory, ParticleFunctionFactory, ForceFactory}
import scala.util.Failure
import scala.Some
import scala.util.Success

object JSON extends Loggable {
  lazy val EMPTY_MAP = apply(Map())
  lazy val EMPTY_LIST = apply(List())

  def apply(any: Any): JSON = new JSON(any)
  def parseFull(json: String): Option[JSON] = scala.util.parsing.json.JSON.parseFull(json).map(apply)

  def parseJSON(item: Any): JSON = JSON(item)
  def parseJSONs(item: Any): List[JSON] = parseList(item).map(parseJSON)

  def parseBoolean(item: Any): Option[Boolean] = Try {
    if (item.isInstanceOf[String]) {
      parseString(item).toLowerCase.startsWith("t")
    } else {
      item.asInstanceOf[Boolean]
    }
  } match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse a Boolean. item='" + item + "'.", ex)
      None
    }
  }
  def parseBooleans(item: Any): List[Boolean] = parseList(item).map(parseBoolean).filter(_.isDefined).map(_.get)

  /**
   * A Time's JSON looks like this:
   * <pre>
   * {
   *   "time" : <long value>
   *   "units" : "<a TimeUnit string>"
   * }
   * </pre>
   * For example:
   * <pre>
   * {
   *   "time" : 500
   *   "units" : "MILLISECONDS"
   * }
   * </pre>
   */
  def parseTime(item: Any): Option[Time] = Try {
    val time = parseJSON(item)
    Time(time.getLong("time").getOrElse(0), TimeUnit.valueOf(time.getString("units").getOrElse(TimeUnit.MILLISECONDS.toString)))
  } match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse a Double. item='" + item + "'.", ex)
      None
    }
  }
  def parseTimes(item: Any): List[Time] = parseList(item).map(parseTime).filter(_.isDefined).map(_.get)

  def parseDouble(item: Any): Option[Double] = Try(item match {
    case value: String => value.toDouble
    case value: Double => value
    case _ => item.toString.toDouble
  }) match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse a Double. item='" + item + "'.", ex)
      None
    }
  }
  def parseDoubles(item: Any): List[Double] = parseList(item).map(parseDouble).filter(_.isDefined).map(_.get)

  def parseLong(item: Any): Option[Long] = Try(item match {
    case value: String => value.toLong
    case value: Double => value.toLong
    case value: Long => value
    case _ => item.toString.toLong
  }) match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse a Long. item='" + item + "'.", ex)
      None
    }
  }
  def parseLongs(item: Any): List[Long] = parseList(item).map(parseLong).filter(_.isDefined).map(_.get)

  def parseInt(item: Any): Option[Int] = Try(item match {
    case value: String => value.toInt
    case value: Double => value.toInt
    case value: Int => value
    case _ => item.toString.toInt
  }) match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse an Int. item='" + item + "'.", ex)
      None
    }
  }
  def parseInts(item: Any): List[Int] = parseList(item).map(parseInt).filter(_.isDefined).map(_.get)

  def parseString(item: Any): String = item.toString
  def parseStrings(item: Any): List[String] = parseList(item).map(parseString)
  def parseStringsList(item: Any): List[List[String]] = parseList(item).map(parseStrings)
  def parseStringsMap(item: Any): Map[String, String] = item.asInstanceOf[Map[String, String]]

  def parseURI(item: Any): Option[URI] = Try(new URI(parseString(item))) match {
    case Success(value) => Option(value)
    case Failure(ex) => {
      error("Failed to parse a URI. item='" + item + "'.", ex)
      None
    }
  }
  def parseURIs(item: Any): List[URI] = parseList(item).map(parseURI).filter(_.isDefined).map(_.get)

  def parseMap(item: Any): Map[String, Any] = Try {
    item.asInstanceOf[Map[String, Any]]
  } match {
    case Success(value) => value
    case Failure(ex) => {
      error("Failed to parse a Map. item='" + item + "'.", ex)
      Map()
    }
  }
  def parseMaps(item: Any): List[Map[String, Any]] = parseList(item).map(parseMap)

  def parseList(item: Any): List[Any] = Try {
    item.asInstanceOf[List[Any]]
  } match {
    case Success(value) => value
    case Failure(ex) => {
      error("Failed to parse a List. item='" + item + "'.", ex)
      List()
    }
  }
  def parseLists(item: Any): List[List[Any]] = parseList(item).map(parseList)

  def parseVector3D(item: Any): Vector3D = Vector3D(parseDoubles(item))
  def parseBounds3D(item: Any): Bounds3D = Bounds3D(parseJSON(item))

  def parseParticle(item: Any): Particle = Particle(parseJSON(item))
  def parseParticles(item: Any): List[Particle] = parseList(item).map(parseParticle)
  def parseParticlesMap(item: Any): Map[String, Particle] = parseMap(item).toList.map {
    case (name, configStr) => (name, parseParticle(configStr))
  }.toMap

  def parseForces(item: Any, particlesMap: Map[String, Particle]): List[Force] = ForceFactory(parseJSON(item), particlesMap).getOrElse(List())
  def parseForcesList(item: Any, particlesMap: Map[String, Particle]): List[List[Force]] = parseList(item).map(item => parseForces(item, particlesMap))

  def parseParticleFunction(item: Any): ParticleFunction = ParticleFunctionFactory(parseJSON(item)).getOrElse(ZeroVector3DParticleFunction)
  def parseVector3DFunction(item: Any): Vector3DFunction = Vector3DFunctionFactory(parseJSON(item)).getOrElse(ZeroVector3DFunction)
}

/**
 * A helper class used to traverse JSON data.
 *
 * For example, assume the "any" object referenced by this object
 * represents the following JSON structure (Maps, Lists, and primitives):
 * <pre>
 * {
 *   "bar" : [
 *     {
 *       "a" : 1,
 *       "b" : 2,
 *       "c" : 3
 *     },
 *     {
 *       "a" : 4,
 *       "b" : 5.0,
 *     },
 *     {
 *       "b" : 6,
 *       "c" : 7,
 *       "d" : 8,
 *       "e" : {
 *         "abc" : "def"
 *         "xyz" : [ "1", "2", "3" ]
 *       }
 *     }
 *   ]
 * }
 * </pre>
 * Here are some examples:
 * <ul>
 * <li>getAny("bar[1].a") returns Some(4)</li>
 * <li>getAny("bar[2].e.abc") returns Some("def")</li>
 * <li>getAny("bar[*].a") returns Some(List(1,4))</li>
 * <li>getAny("unknown") returns None</li>
 * </ul>
 *
 * Typically you'll want the result to be a specific type. In that case, you can call a type-specific method.
 * For example:
 * <ul>
 * <li>getInt("bar[1].a") returns a Some[Int] with the value Some(4)</li>
 * <li>getString("bar[2].e.abc") returns a Some[String] with the value Some("def")</li>
 * <li>getInts("bar[*].a") returns a Some[List[Int]] with the value Some(List(1,4))</li>
 * <li>getString("unknown") returns None</li>
 * <li>getStrings("bar[2].e.xyz") returns a Some[List[String]] with the value Some(List("1","2","3"))</li>
 * </ul>
 *
 * @param any a reference to a Map, List, or primitive type.
 */
class JSON(any: Any) extends Loggable {
  /**
   * Returns the value referenced by the specified key wrapped in an Option.
   * If this object does not contain an object with the specified key, this method returns None.
   * If this object does contain an object with the specified key, the object is returned as a Some[Any].
   * The caller is assumed to know the underlying type and structure of this object's "any" and can therefore
   * create keys to reference children or ancestors of this object. See the docs for this class for examples of
   * how to use the getAny method.
   *
   * @param key the key identifying the object to return
   * @return the object referenced by the specified key
   */
  def getAny(key: String): Option[Any] = if (key.isEmpty) {
    Option(any)
  } else if (key.startsWith(".")) {
    getAny(key.substring(1))
  } else if (key.startsWith("[")) {
    val closingBracket = key.indexOf("]")
    val nextKey = key.substring(closingBracket + 1)
    val list = JSON.parseList(any)
    val indexStr = key.substring(1, closingBracket)
    if (indexStr == "*") {
      Some(list.map(JSON(_)).map(_.getAny(nextKey)).filter(_.isDefined).map(_.get))
    } else {
      val index = indexStr.toInt
      if (index >= 0 || index < list.size) {
        JSON(list(index)).getAny(nextKey)
      } else {
        None
      }
    }
  } else {
    val nextDot = key.indexOf('.')
    val nextBracket = key.indexOf('[')
    val nextThing = if (nextDot < 0 && nextBracket < 0) {
      key.length
    } else if (nextDot < 0) {
      nextBracket
    } else if (nextBracket < 0) {
      nextDot
    } else {
      nextDot.min(nextBracket)
    }

    val subKey = key.substring(0, nextThing)
    val nextKey = key.substring(nextThing)
    val map = JSON.parseMap(any)
    if (map.contains(subKey)) {
      JSON(map(subKey)).getAny(nextKey)
    } else {
      None
    }
  }

  /**
   * Returns the object referenced by the specified index wrapped in an Option.
   * Assuming this object's underlying object (any) is a List[Any]. If the index
   * is invalid, this method return None.
   *
   * @param index the list index
   * @return the object referenced by the specified index wrapped in an Option. If the index
   *         is invalid, this method return None.
   */
  def getAny(index: Int): Option[Any] = {
    val list = JSON.parseList(any)
    if (index >= 0 && index < list.size) {
      Option(list(index))
    } else {
      None
    }
  }

  /**
   * If this object represents a Map, this method returns the map's keys; otherwise
   * it throws an exception.
   *
   * @return this object's keys, if this object represents a map
   */
  def getKeys: List[String] = JSON.parseMap(any).keys.toList

  /**
   * Helper functions that return the object referenced by the specified key as a specific type.
   *
   * @see getAny(key: String)
   *
   * @param key the key
   * @return the object referenced by the specified key as a specific type.
   */
  def getJSON(key: String)      : Option[JSON]                   = getAny(key).map(JSON.parseJSON)
  def getJSONs(key: String)     : Option[List[JSON]]             = getAny(key).map(JSON.parseJSONs)
  def getString(key: String)    : Option[String]                 = getAny(key).map(JSON.parseString)
  def getStrings(key: String)   : Option[List[String]]           = getAny(key).map(JSON.parseStrings)
  def getDouble(key: String)    : Option[Double]                 = getAny(key).flatMap(JSON.parseDouble)
  def getDoubles(key: String)   : Option[List[Double]]           = getAny(key).map(JSON.parseDoubles)
  def getInt(key: String)       : Option[Int]                    = getAny(key).flatMap(JSON.parseInt)
  def getInts(key: String)      : Option[List[Int]]              = getAny(key).map(JSON.parseInts)
  def getLong(key: String)      : Option[Long]                   = getAny(key).flatMap(JSON.parseLong)
  def getLongs(key: String)     : Option[List[Long]]             = getAny(key).map(JSON.parseLongs)
  def getBoolean(key: String)   : Option[Boolean]                = getAny(key).flatMap(JSON.parseBoolean)
  def getBooleans(key: String)  : Option[List[Boolean]]          = getAny(key).map(JSON.parseBooleans)
  def getTime(key: String)      : Option[Time]                   = getAny(key).flatMap(JSON.parseTime)
  def getTimes(key: String)     : Option[List[Time]]             = getAny(key).map(JSON.parseTimes)
  def getURI(key: String)       : Option[URI]                    = getAny(key).flatMap(JSON.parseURI)
  def getURIs(key: String)      : Option[List[URI]]              = getAny(key).map(JSON.parseURIs)
  def getMap(key: String)       : Option[Map[String, Any]]       = getAny(key).map(JSON.parseMap)
  def getMaps(key: String)      : Option[List[Map[String, Any]]] = getAny(key).map(JSON.parseMaps)
  def getList(key: String)      : Option[List[Any]]              = getAny(key).map(JSON.parseList)
  def getLists(key: String)     : Option[List[List[Any]]]        = getAny(key).map(JSON.parseLists)
  def getStringsMap(key: String): Option[Map[String, String]]    = getAny(key).map(JSON.parseStringsMap)
  def getStringsList(key: String) : Option[List[List[String]]]  = getAny(key).map(JSON.parseStringsList)

  def getParticle(key: String)  : Option[Particle]               = getAny(key).map(JSON.parseParticle)
  def getParticles(key: String) : Option[List[Particle]]         = getAny(key).map(JSON.parseParticles)
  def getParticlesMap(key: String) : Option[Map[String, Particle]] = getAny(key).map(JSON.parseParticlesMap)
  def getForcesList(key: String, particlesMap: Map[String, Particle]) : Option[List[List[Force]]] =
    getAny(key).map(any => JSON.parseForcesList(any, particlesMap))
  def getVector3D(key: String)  : Option[Vector3D]               = getAny(key).map(JSON.parseVector3D)
  def getBounds3D(key: String)  : Option[Bounds3D]               = getAny(key).map(JSON.parseBounds3D)
  def getParticleFunction(key: String) : Option[ParticleFunction] = getAny(key).map(JSON.parseParticleFunction)
  def getVector3DFunction(key: String) : Option[Vector3DFunction] = getAny(key).map(JSON.parseVector3DFunction)

  /**
   * Helper functions that return the object referenced by the specified index as a specific type.
   *
   * @see getAny(index: Int)
   *
   * @param index the index
   * @return the object referenced by the specified index as a specific type.
   */
  def getJSON(index: Int)       : Option[JSON]                   = getAny(index).map(JSON.parseJSON)
  def getJSONs(index: Int)      : Option[List[JSON]]             = getAny(index).map(JSON.parseJSONs)
  def getString(index: Int)     : Option[String]                 = getAny(index).map(JSON.parseString)
  def getStrings(index: Int)    : Option[List[String]]           = getAny(index).map(JSON.parseStrings)
  def getDouble(index: Int)     : Option[Double]                 = getAny(index).flatMap(JSON.parseDouble)
  def getDoubles(index: Int)    : Option[List[Double]]           = getAny(index).map(JSON.parseDoubles)
  def getInt(index: Int)        : Option[Int]                    = getAny(index).flatMap(JSON.parseInt)
  def getInts(index: Int)       : Option[List[Int]]              = getAny(index).map(JSON.parseInts)
  def getLong(index: Int)       : Option[Long]                   = getAny(index).flatMap(JSON.parseLong)
  def getLongs(index: Int)      : Option[List[Long]]             = getAny(index).map(JSON.parseLongs)
  def getBoolean(index: Int)    : Option[Boolean]                = getAny(index).flatMap(JSON.parseBoolean)
  def getBooleans(index: Int)   : Option[List[Boolean]]          = getAny(index).map(JSON.parseBooleans)
  def getTime(index: Int)       : Option[Time]                   = getAny(index).flatMap(JSON.parseTime)
  def getTimes(index: Int)      : Option[List[Time]]             = getAny(index).map(JSON.parseTimes)
  def getURI(index: Int)        : Option[URI]                    = getAny(index).flatMap(JSON.parseURI)
  def getURIs(index: Int)       : Option[List[URI]]              = getAny(index).map(JSON.parseURIs)
  def getMap(index: Int)        : Option[Map[String, Any]]       = getAny(index).map(JSON.parseMap)
  def getMaps(index: Int)       : Option[List[Map[String, Any]]] = getAny(index).map(JSON.parseMaps)
  def getList(index: Int)       : Option[List[Any]]              = getAny(index).map(JSON.parseList)
  def getLists(index: Int)      : Option[List[List[Any]]]        = getAny(index).map(JSON.parseLists)
  def getStringsList(index: Int) : Option[List[List[String]]]    = getAny(index).map(JSON.parseStringsList)
  def getStringsMap(index: Int) : Option[Map[String, String]]    = getAny(index).map(JSON.parseStringsMap)

  def asMap = JSON.parseMap(any)
  def asList = JSON.parseList(any)
  def asLists = JSON.parseLists(any)
}
