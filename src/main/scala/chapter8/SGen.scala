package chapter8

/**
  * Created by paul on 8/29/16.
  */
case class SGen[+A](forSize: Int => Gen[A])
