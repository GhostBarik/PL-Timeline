import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.model.{Document, Element}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._

import scala.collection.parallel.{ForkJoinTaskSupport, TaskSupport}
import scala.collection.parallel.immutable.ParSeq

import scala.collection.immutable.Set
import scala.collection.immutable.Map

import util.Properties

/**
  * Created by ghostbarik on 7/12/16.
  */
object Main {

  // type alias for URL
  type URL = String

  // url of the wikipedia main domain
  val wikiDomain = "https://en.wikipedia.org"

  var visitedUrls = Set[URL]()
  var graphOfLanguages = Map[URL, Set[URL]]()
  var languageNameByUrl = Map[URL, String]()

  implicit val executionContext = new ForkJoinTaskSupport(
    new scala.concurrent.forkjoin.ForkJoinPool(8)
  )

  def main(args: Array[String]) {

    // starting page
    val firstPage = WikiPage(wikiDomain + "/wiki/Java_(programming_language)", external = false, "Java")

    scanPages(firstPage, fetchHtml(firstPage.url))

    println(toJsFormat(graphOfLanguages))
  }

  def toDotFormat(graph: Map[URL, Set[URL]]): String = {

    def escapeItem(s: String) = "\"" + s.replace("\"\"", "") + "\""

    val escaped = graph.map{ case (k, v) =>
      languageNameByUrl(k) -> v.map(languageNameByUrl)
    }

    val res = for ((k,v) <- escaped; elem <- v)
      yield escapeItem(k) + " -> " + escapeItem(elem) + ";"

    "digraph G {" + Properties.lineSeparator + res.mkString(Properties.lineSeparator) + "}"
  }

  def toJsFormat(graph: Map[URL, Set[URL]]): String = {

    def escapeItem(s: String) = "\"" + s.replace("\"\"", "") + "\""

    val escaped = graph.map{ case (k, v) =>
      languageNameByUrl(k) -> v.map(languageNameByUrl)
    }

    val allKeys = escaped.keySet ++ escaped.values.flatten.toSet

    val nodes = allKeys.map { k =>
      val esc = escapeItem(k)
      s"{ data: { id: $esc, name: $esc } }"
    }

    val edges = for ((k,v) <- escaped; elem <- v)
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

  def scanPages(startPage: WikiPage, pageContent: Document): Unit = {

    val nextPages: List[WikiPage] = parseHtml(pageContent)

    languageNameByUrl += (startPage.url -> startPage.language)

    val parsed = parsePages(startPage, nextPages.filter{ nextPage =>

      linkTwoPages(startPage, nextPage)

      languageNameByUrl += (nextPage.url -> nextPage.language)

        !(visitedUrls contains nextPage.url)

      }.filter(!_.external))

      for ((d,w) <- parsed) {

        if (w.url == "https://en.wikipedia.org/wiki/R_(programming_language)") {
          val a = "test"
        }
        scanPages(w,d)
      }
  }


  def linkTwoPages(first: WikiPage, second: WikiPage) = {

    graphOfLanguages.get(first.url) match {

      case Some(l: Set[URL]) =>
        graphOfLanguages += (first.url -> (l + second.url))

      case None =>
        graphOfLanguages += (first.url -> Set(second.url))
    }
  }



  case class WikiPage(url: URL, external: Boolean, language: String)

  def fetchHtml(url: URL): Document = JsoupBrowser().get(url)

  def parsePages(from: WikiPage, nextPages: List[WikiPage]): List[(Document, WikiPage)] = {

    val results = inParallel(nextPages).map{ p =>

      this.synchronized {
        visitedUrls += p.url
        println("visiting: " + p.url)
      }

      (fetchHtml(p.url), p)

    }.seq

    results.toList
  }

  def inParallel[E](l: List[E])(implicit ts: TaskSupport): ParSeq[E] = {
    val pr = l.par
    pr.tasksupport = ts
    pr
  }

  // return -> List[influenced]
  def parseHtml(html: Document): List[WikiPage] = {

    val rightContentBlock = html >> elementList("#mw-content-text > table.infobox.vevent > tbody > tr")

    val influenced = rightContentBlock.zipWithIndex.find{ case (elem, _) =>
      (elem.innerHtml contains "Influenced") &&  !(elem.innerHtml contains "Influenced by")
    }

    influenced.map{ case (_,index) =>

      val (_,index) = influenced.get

      val languages = rightContentBlock(index + 1) >> elementList("a")

      def unify(s: String): String = s
        .replace("(programming_language)", "programming_language")
        .replace("programming_language", "(programming_language)")

      for {
        elem <- languages
        if !(elem.attr("href") contains "cite_note")
        if !(elem.attr("href") contains "Citation_needed")
        if !elem.attrs.get("class").contains("new")

      } yield WikiPage(

        wikiDomain + unify(elem.attr("href")),
        !elem.attr("href").contains("/wiki"),
        elem.innerHtml
      )

    }.getOrElse(List.empty)
  }
}
