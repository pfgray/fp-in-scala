package chapter10

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author pgray
  */
class WCTest extends FlatSpec with Matchers {

  "WC" should "count correctly" in {
    val words =
      """
        |Lorem ipsum dolor sit amet, consectetur adipiscing elit.
        |Morbi volutpat vitae mauris nec suscipit.
        |Donec in convallis sem, vel dictum tellus.
        |Donec efficitur lorem non libero tempus feugiat.
        |Donec vestibulum purus ac dignissim luctus.
        |Nullam sit amet leo sed massa sodales sodales quis vel ante.
        |Phasellus non lorem efficitur, tempor orci non, fermentum justo.
        |Maecenas laoreet elit mattis neque convallis pharetra.
        |Proin sed bibendum ante. Mauris laoreet fermentum convallis.
        |Interdum et malesuada fames ac ante ipsum primis in faucibus.
        |Morbi aliquam arcu libero.
        |Praesent at est auctor, rhoncus lorem ut, gravida urna.
        |In feugiat purus molestie risus lacinia, eget tincidunt dui cursus.
        |Integer luctus ligula tellus, vel luctus mi auctor vitae.
        |Aliquam feugiat non tortor a congue.
      """.stripMargin

    WC.countWords(words) should be(Part("", 117, ""))
  }

}
