package nl.up.property.resolver
import scala.collection.mutable.{ Map => MMap }
import java.util.Properties
import scala.io.Source
import java.io.File
import scala.io.Codec

/**
 * Trait to resolve placeholders in property files.
 */
trait PropertyResolver {

  private val PLACEHOLDER_PATTERN = """\$\{([^\}]+)\}"""
  private val PlaceHoldersRegexp = PLACEHOLDER_PATTERN.r
  private val PlaceHolderRegexp = (".*%s.*" format PLACEHOLDER_PATTERN).r
  private[resolver] def filterPlaceHolders(txt: String) = PlaceHoldersRegexp.findAllIn(txt).matchData.flatMap { _.subgroups } toList

  /**
   * Resolves placeholders with the format ${my.place.holder} contained in the value of one or many maps.
   * Example 1 single reference:
   * db.url=${prod.db.config}
   * prod.db.config=jdbc:oracle:thin
   *
   * Output:
   * db.url=jdbc:oracle:thin
   * prod.db.config=jdbc:oracle:thin
   *
   * Example 2 multiple references:
   * name=${first.name} ${last.name}
   * first.name=Tom
   * last.name=Hanks
   *
   * Output:
   * name=Tom Hanks
   * first.name=Tom
   * last.name=Hanks
   *
   * Example 3 chained references:
   * host=${host.ref}
   * host.ref=${test.host}
   * test.host=test.server.com
   *
   * Output:
   * host=test.server.com
   * host.ref=test.server.com
   * test.host=test.server.com
   */
  def resolve(inputMaps: Map[String, String]*): Map[String, String] = {
    def containsMoreRefs(input: Map[String, String]): Boolean = {
      val hasPlaceHolder = (txt: String) => PlaceHolderRegexp.pattern.matcher(txt).matches()
      input.values.exists(hasPlaceHolder)
    }

    val combinedMap = Map(inputMaps.toSeq.flatten: _*)
    val solved = solve(combinedMap)
    if (containsMoreRefs(solved)) {
      resolve(solved)
    } else {
      solved
    }
  }

  private def solve(input: Map[String, String]): Map[String, String] = {
    def checkCyclicRefs(input: MMap[String, String]) = {
      if(input.exists { case (key, value) => filterPlaceHolders(value).contains(key) })
        throw new IllegalArgumentException("Input contains cyclic references") 
    }

    val tmpMMap = MMap(input.toSeq: _*)
    for (
      key <- input.keys;
      val value = input(key);
      placeholder <- filterPlaceHolders(value);
      val resolvedPlaceholder = tmpMMap(placeholder)
    ) {
      tmpMMap.update(key, tmpMMap(key).replace("${%s}" format placeholder, resolvedPlaceholder))
    }
    checkCyclicRefs(tmpMMap)
    tmpMMap toMap
  }
}


/**
 * Trait to load property files and combine them into a Map
 */
trait PropertyLoader {
  import scala.collection.JavaConversions._
  /**
   * Load properties file(s) and convert them into a Map
   */
  def load(paths: String*): Map[String, String] = {
    val maps = paths.map { loadFromPath }
    Map(maps.toSeq.flatten: _*)
  }

  private def loadFromPath(path: String): MMap[String, String] = {
    val prop = new Properties()
    val file = new File(path)
    if (file.exists()) {
      prop.load(Source.fromFile(file)(Codec.UTF8).bufferedReader())
    } else {
      prop.load(Thread.currentThread().getContextClassLoader().getResourceAsStream(path))
    }
    prop
  }
}

/**
 * Resolver combining all relevant traits
 */
object PropertyResolver extends PropertyResolver with PropertyLoader

/**
 * Main class
 */
object PropertyResolverMain {
  import PropertyResolver._
  def main(args: Array[String]): Unit = {
    val maps = load(args: _*)
    val resolved = resolve(maps)
    resolved foreach { case (k, v) => println("%s=%s" format (k, v)) }
  }
}
