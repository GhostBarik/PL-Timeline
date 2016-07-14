/**
  * Created by ghostbarik on 7/14/2016.
  */
object Model {

  // type alias for URL
  type URL = String

  /**
    * wikipedia page about programming language
    */
  case class WikiPage(url: URL, external: Boolean, language: String)
}
