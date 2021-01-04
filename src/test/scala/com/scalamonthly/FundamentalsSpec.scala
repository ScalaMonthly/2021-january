package com.scalamonthly

import cats.syntax.all._
import cats.data.NonEmptyList

final class FundamentalsSpec extends munit.FunSuite {

    import fundamentals._

    private implicit class ParserResultOps[E, A, B](i: Either[E, (A, B)]) {
        def resultOnly: Either[E, B] = i.map(_._2)
    }

    test("one - should parse") {
        import Binary._
        val inputs = List("0", "1")
        val outputs = inputs.traverse(one.parse(_).resultOnly)
        val expectedOutput = List(Zero, One)
        assertEquals(outputs, Right(expectedOutput))
    }

    test("one - should NOT parse") {
        val inputs = List("", "2", " ")
        val outputs = inputs.map(one.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("two - should parse") {
        val inputs = List("A1", "a2")
        val outputs = inputs.traverse(two.parse(_).resultOnly)
        val expectedOutput = List(LetterAndNumber("A1"), LetterAndNumber("a2"))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("two - should NOT parse") {
        val inputs = List("", "1A", "A 1", "aa", "11", " ", "%")
        val outputs = inputs.map(two.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("three - should parse") {
        import Binary._
        val inputs = List("1011", "00", "1 this will still parse")
        val outputs = inputs.traverse(three.parse(_).resultOnly)
        val expectedOutput = List(BinaryList(NonEmptyList.of(One, Zero, One, One)),
                                  BinaryList(NonEmptyList.of(Zero, Zero)), BinaryList(NonEmptyList.of(One)))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("three - should NOT parse") {
        val inputs = List("234", "A 1", "aa", " ", "%")
        val outputs = inputs.map(three.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("four - should parse") {
        val inputs = List("Bob Dylan", "Bob O'Brien")
        val outputs = inputs.traverse(four.parse(_).resultOnly)
        val expectedOutput = List(Name("Bob Dylan"), Name("Bob O'Brien"))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("four - should NOT parse") {
        val inputs = List(" My Name", "'Bob", "", "1Bob", "%")
        val outputs = inputs.map(four.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("five - should parse") {
        val inputs = List("0 - 0", "125 - 129", "0125 - 1290")
        val outputs = inputs.traverse(five.parse(_).resultOnly)
        val expectedOutput = List(Score(0, 0), Score(125, 129), Score(125, 1290))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("five - should NOT parse") {
        val inputs = List(" 2 - 1", "2-2", "1- 2", "1 -0", "11-", " ", "1  -  9", "%")
        val outputs = inputs.map(five.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("six - should parse") {
        val inputs = List("one,two", "o,t,th", "a,b", "a,b,c")
        val outputs = inputs.traverse(six.parse(_).resultOnly)
        val expectedOutput = List(MyTuple.Two("one", "two"), MyTuple.Three("o", "t", "th"),
                                  MyTuple.Two("a", "b"), MyTuple.Three("a", "b", "c"))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("six - should NOT parse") {
        val inputs = List("", "a", "a ,b", "abc", "123", " ", "%")
        val outputs = inputs.map(six.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("seven - should parse") {
        val inputs = List("Nick.Sean.Miller", "Jess.Day", "Schmidt", "Bear.Claw.The.Original.Wolf.aka.Josh.Gad")
        val outputs = inputs.traverse(seven.parse(_).resultOnly)
        val expectedOutput = List(UserName("Nick.Sean.Miller"), UserName("Jess.Day"),
                                  UserName("Schmidt"), UserName("Bear.Claw.The.Original.Wolf.aka.Josh.Gad"))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("seven - should NOT parse") {
        val inputs = List("..", ".lastname", "", "_", "1.1", " ", "testing.")
        val outputs = inputs.map(seven.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("eight - should parse") {
        val inputs = List("$#@!", "####", "!", "!@@@$")
        val outputs = inputs.traverse(eight(List('!', '@', '#', '$')).parse(_).resultOnly)
        val expectedOutput = inputs
        assertEquals(outputs, Right(expectedOutput))
    }

    test("eight - should NOT parse") {
        val inputs = List("", "other.", "1.1", " ")
        val outputs = inputs.map(eight(List('.')).parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

    test("nine - should parse") {
        val inputs = List("Honda Accord", "Fiat")
        val outputs = inputs.traverse(nine.parse(_).resultOnly)
        val expectedOutput = List(CarType("Honda", Some("Accord")), CarType("Fiat", None))
        assertEquals(outputs, Right(expectedOutput))
    }

    test("nine - should NOT parse") {
        val inputs = List("", ".other", "2", " ")
        val outputs = inputs.map(nine.parse(_).resultOnly)
        assert(outputs.forall(_.isLeft), "all of these inputs should fail to parse")
    }

}