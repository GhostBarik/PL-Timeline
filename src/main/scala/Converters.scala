import Model.URL

import scala.collection.immutable.{Map, Set}
import scala.util.Properties

/**
  * Created by ghostbarik on 7/14/2016.
  */
object Converters {

  def toDotFormat(graph: Map[String, Set[String]]): String = {

    def escapeItem(s: String) = "\"" + s.replace("\"\"", "") + "\""

    val res = for ((k,v) <- graph; elem <- v)
      yield escapeItem(k) + " -> " + escapeItem(elem) + ";"

    "digraph G {" + Properties.lineSeparator + res.mkString(Properties.lineSeparator) + "}"
  }

  def toJsFormat(graph: Map[String, Set[String]]): String = {

    def escapeItem(s: String) = "\"" + s.replace("\"\"", "") + "\""

    val allKeys = graph.keySet ++ graph.values.flatten.toSet

    val nodes = allKeys.map { k =>
      val esc = escapeItem(k)
      s"{ data: { id: $esc, name: $esc } }"
    }

    val edges = for ((k,v) <- graph; elem <- v)
      yield {
        val escK = escapeItem(k)
        val escV = escapeItem(elem)
        s"{ data: { source: $escK, target: $escV } }"
      }

    def makeObject(key: String, inner: String) = Seq(s"$key: {", inner, "}")
      .mkString("", Properties.lineSeparator, Properties.lineSeparator)

    def makeArray(key: String, inner: String) = Seq(s"$key: [", inner, "]")
      .mkString("", Properties.lineSeparator, Properties.lineSeparator)

    makeObject("elements",
      makeArray("nodes", nodes.mkString("," + Properties.lineSeparator)) + "," +
        makeArray("edges", edges.mkString("," + Properties.lineSeparator)))
  }
}
