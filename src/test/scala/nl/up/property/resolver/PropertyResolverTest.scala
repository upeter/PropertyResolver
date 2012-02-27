package nl.up.property.resolver

import org.junit.runner.RunWith

import org.specs2.matcher._
import org.specs2.mutable._

import scala.collection.immutable.TreeMap
import org.specs2.runner.JUnitRunner

 @RunWith(classOf[JUnitRunner])
class PropertyResolverTest extends Specification /*with ScalaCheck*/ {

  import PropertyResolver._

  "PropertyResolver" should {
    "filter placeholders" in {
      val txt = "good ${day} Mr. ${initals} ${lastname}, how are you today?"
      filterPlaceHolders(txt) must_== (List("day", "initals", "lastname"))
    }
    "resolve chained cyclic references" in {
      val props = Map(
        ("a" -> "a points to ${b}"), 
        ("b" -> "b points to ${c}"),
        ("c" -> "c points to ${a}"))
      resolve(props) must throwA[IllegalArgumentException]
    }
    "resolve cyclic references" in {
      val props = Map(
        ("a" -> "Hoe dacht het je dit precies op te lossen ${a}."),
        ("b" -> "dit precies op te lossen Hoe dacht het je ${b}."))
      resolve(props) must throwA[IllegalArgumentException]
    }
    "resolve single reference" in {
      val props = Map(
        ("jndiname" -> "${jndi.ref}"),
        ("jndi.ref" -> "MyQueue"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("jndi.ref", "MyQueue"), ("jndiname", "MyQueue"))

    }
    "resolve chained references" in {
      val props = Map(
        ("a" -> "link to ${b}"),
        ("b" -> "another link to ${c}"),
        ("c" -> "target"))
      val resolved = resolve(props)
      println(resolved)
      sort(resolved) must_== TreeMap(("c", "target"), ("a", "link to another link to target"), ("b", "another link to target"))

    }
    "resolve multiple references" in {
      val props = Map(
        ("name" -> "${first.name} ${last.name}"),
        ("first.name" -> "Elton"),
        ("last.name" -> "John"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("first.name" -> "Elton"), ("last.name" -> "John"), ("name", "Elton John"))

    }
    "resolve placholders spread in multipe maps" in {
      val template = Map(
        ("name" -> "${first.name} ${last.name}"))
      val values = Map(
        ("first.name" -> "Elton"),
        ("last.name" -> "John"))
      val resolved = resolve(template, values)
      sort(resolved) must_== TreeMap(("first.name" -> "Elton"), ("last.name" -> "John"), ("name", "Elton John"))
    }
    "resolve placholders spread in multipe properties files" in {
      val props = load("template.properties", "prod.properties")
      val resolved = resolve(props)
      val expected = TreeMap(("db.url" -> "jdbc:oracle:thin"), ("prod.db.config" -> "jdbc:oracle:thin"), ("first.name" -> "Tom"), ("last.name" -> "Hanks"), ("name", "Tom Hanks"), ("host", "test.server.com"), ("host.ref" -> "test.server.com"), ("test.host" -> "test.server.com"))
      sort(resolved) must_== expected
    }
  }

  def sort(map: Map[String, String]) = TreeMap(map.toSeq: _*)
}

