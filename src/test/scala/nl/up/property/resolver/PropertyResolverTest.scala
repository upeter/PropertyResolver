package nl.up.property.resolver

import org.junit.runner.RunWith
import org.specs._
import org.specs.matcher._
import org.specs.runner.{ JUnitSuiteRunner, JUnit }
import scala.collection.immutable.TreeMap

@RunWith(classOf[JUnitSuiteRunner])
class PropertyResolverTest extends Specification with JUnit /*with ScalaCheck*/ {

  import PropertyResolver._

  "PropertyResolver" should {
    "filter placeholders" in {
      val txt = "good ${day} Mr. ${initals} ${lastname}, how are you today?"
      filterPlaceHolders(txt) must_== (List("day", "initals", "lastname"))
    }
    "resolve single reference" in {
      val props = Map(
        ("jndiname" -> "${jndi.ref}"),
        ("jndi.ref" -> "MyQueue"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("jndi.ref", "MyQueue"), ("jndiname", "MyQueue"))

    }
    "resolve multiple references" in {
      val props = Map(
        ("name" -> "${first.name} ${last.name}"),
        ("first.name" -> "Elton"),
        ("last.name" -> "John"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("first.name" -> "Elton"), ("last.name" -> "John"), ("name", "Elton John"))

    }
    "resolve chained references" in {
      val props = Map(
        ("host" -> "${host.ref}"),
        ("host.ref" -> "${prod.config}"),
        ("prod.config" -> "xebia.com"))
      val resolved = resolve(props)
      sort(resolved) must_== TreeMap(("host", "xebia.com"), ("host.ref" -> "xebia.com"), ("prod.config" -> "xebia.com"))

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

