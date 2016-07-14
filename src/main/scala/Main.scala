import Model.{URL, WikiPage}
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Document

import scala.collection.immutable.{Map, Set}
import scala.collection.parallel.TaskSupport
import scala.collection.parallel.immutable.ParSeq

/**
  * Main script
  */
object Main {

  import Config.executionContext

  // url of the wikipedia main domain
  val wikiDomain = "https://en.wikipedia.org"

  var visitedUrls = Set[URL]()
  var graphOfLanguages = Map[URL, Set[URL]]().withDefault(_ => Set.empty[URL])
  var languageNameByUrl = Map[URL, String]()

  def fetchHtml(url: URL): Document = JsoupBrowser().get(url)

  /**
    * script entry point
    */
  def main(args: Array[String]) {

    // starting page
    val firstPage = WikiPage(wikiDomain + "/wiki/Java_(programming_language)", external = false, "Java")

    scanPages(firstPage, fetchHtml(firstPage.url))

    val escaped: Map[String, Set[String]] = graphOfLanguages.map{ case (k, v) =>
      languageNameByUrl(k) -> v.map(languageNameByUrl)
    }

    println(Converters.toJsFormat(escaped))
  }

  def scanPages(startPage: WikiPage, pageContent: Document): Unit = {

    val nextPages: List[WikiPage] = parseHtml(pageContent)

    languageNameByUrl += (startPage.url -> startPage.language)

    val parsed = parsePages(startPage, nextPages.filter{ nextPage =>

      //linkTwoPages(startPage, nextPage)
      graphOfLanguages += (startPage.url -> (graphOfLanguages(startPage.url) + nextPage.url))

      languageNameByUrl += (nextPage.url -> nextPage.language)

        !(visitedUrls contains nextPage.url)

      }.filter(!_.external))

      for ((d,w) <- parsed) {
        scanPages(w,d)
      }
  }

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
