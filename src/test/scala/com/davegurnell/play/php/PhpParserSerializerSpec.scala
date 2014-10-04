package com.davegurnell.play.php

/*
 * Adapted from the `serialized-php-parser` Java library:
 *
 *     https://code.google.com/p/serialized-php-parser/
 *     Copyright (c) 2007 Zsolt Szász <zsolt at lorecraft dot com>
 *     MIT License: http://opensource.org/licenses/mit-license.php
 */

import java.io._
import org.scalatest._

class PhpParserSerializerTest extends WordSpec with Matchers {
  def createParser(str: String) =
    new PhpParser(new ByteArrayInputStream(str.getBytes))

  def serialize(value: PhpValue): String =
    new String(PhpSerializer(value))

  def deserialize(str: String): PhpValue =
    PhpParser(str)

  "PhpParser.read" should {
    "read a single byte" in {
      val parser = createParser("abc")
      parser.read() should equal('a'.toByte)
      parser.read() should equal('b'.toByte)
      parser.read() should equal('c'.toByte)
      parser.read() should equal(-1.toByte)
    }
  }

  "PhpParser and PhpSerializer" should {
    "handle a null" in {
      val serialized = "N;"
      val deserialized = PhpNull
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle an int" in {
      val serialized = "i:123;"
      val deserialized = PhpInt(123)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a double" in {
      val serialized = "d:123.456;"
      val deserialized = PhpDouble(123.456)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle an int that is too large for the Int type (1)" in {
      val serialized = "i:3422865137422183;"
      val deserialized = PhpDouble(3.422865137422183E15)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal("d:3.422865137422183E15;")
    }

    "handle an int that is too large for the Int type (2)" in {
      val serialized = "i:100010001804;"
      val deserialized = PhpDouble(1.00010001804E11)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal("d:1.00010001804E11;")
    }

    "handle a true boolean" in {
      val serialized = "b:1;"
      val deserialized = PhpBoolean(true)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a falue boolean" in {
      val serialized = "b:0;"
      val deserialized = PhpBoolean(false)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a string" in {
      val serialized = "s:6:\"string\";"
      val deserialized = PhpString("string")
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a simple array" in {
      val serialized = "a:1:{i:1;i:2;}"
      val deserialized = Php.arr(1 -> 2)
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a simple object" in {
      val serialized = "O:8:\"TypeName\":1:{s:3:\"foo\";s:3:\"bar\";}"
      val deserialized = Php.obj("TypeName")("foo" -> "bar")
      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a complex data structure" in {
      val serialized =
        """
        a:2:{
          i:0;a:8:{
            s:5:"class";O:7:"MyClass":1:{
              s:5:"pippo";s:4:"test";
            }
            i:0;i:1;
            i:1;d:0.19999998807907104;
            i:2;b:1;
            i:3;b:0;
            i:4;N;
            i:5;a:1:{
              i:0;s:1:";";
            }
            i:6;O:6:"Object":0:{}
          }
          i:1;a:8:{
            s:5:"class";O:7:"MyClass":1:{
              s:5:"pippo";
              s:4:"test";
            }
            i:0;i:1;
            i:1;d:0.19999998807907104;
            i:2;b:1;
            i:3;b:0;
            i:4;N;
            i:5;a:1:{
              i:0;s:1:";";
            }
            i:6;O:6:"Object":0:{}
          }
        }
        """.lines.map(_.trim).mkString.trim

      val deserialized =
        Php.arr(
          0 -> Php.arr(
            "class" -> Php.obj("MyClass")(
              "pippo" -> "test"
            ),
            0 -> 1,
            1 -> 0.19999998807907104,
            2 -> true,
            3 -> false,
            4 -> PhpNull,
            5 -> Php.arr(
              0 -> ";"
            ),
            6 -> Php.obj("Object")(
            )
          ),
          1 -> Php.arr(
            "class" -> Php.obj("MyClass")(
              "pippo" -> "test"
            ),
            0 -> 1,
            1 -> 0.19999998807907104,
            2 -> true,
            3 -> false,
            4 -> PhpNull,
            5 -> Php.arr(
              0 -> ";"
            ),
            6 -> Php.obj("Object")(
            )
          )
        )

        deserialize(serialized) should equal(deserialized)
        serialize(deserialized) should equal(serialized)
      }

    "parse the output of a yahoo web image search api call" in {
      val serialized =
        """
        a:1:{
          s:9:"ResultSet";a:4:{
            s:21:"totalResultsAvailable";s:7:"1177824";
            s:20:"totalResultsReturned";i:2;
            s:19:"firstResultPosition";i:1;
            s:6:"Result";a:2:{
              i:0;a:10:{
                s:5:"Title";s:12:"corvette.jpg";
                s:7:"Summary";s:150:"bluefirebar.gif 03-Nov-2003 19:02 22k burning_frax.jpg 05-Jul-2002 14:34 169k corvette.jpg 21-Jan-2004 01:13 101k coupleblack.gif 03-Nov-2003 19:00 3k";
                s:3:"Url";s:48:"http://www.vu.union.edu/~jaquezk/MG/corvette.jpg";
                s:8:"ClickUrl";s:48:"http://www.vu.union.edu/~jaquezk/MG/corvette.jpg";
                s:10:"RefererUrl";s:35:"http://www.vu.union.edu/~jaquezk/MG";
                s:8:"FileSize";s:7:"101.5kB";
                s:10:"FileFormat";s:4:"jpeg";
                s:6:"Height";s:3:"768";
                s:5:"Width";s:4:"1024";
                s:9:"Thumbnail";a:3:{
                  s:3:"Url";s:42:"http://sp1.mm-a1.yimg.com/image/2178288556";
                  s:6:"Height";s:3:"120";
                  s:5:"Width";s:3:"160";
                }
              }
              i:1;a:10:{
                s:5:"Title";s:23:"corvette_c6_mini_me.jpg";
                s:7:"Summary";s:48:"Corvette I , Corvette II , Diablo , Enzo , Lotus";
                s:3:"Url";s:54:"http://www.ku4you.com/minicars/corvette_c6_mini_me.jpg";
                s:8:"ClickUrl";s:54:"http://www.ku4you.com/minicars/corvette_c6_mini_me.jpg";
                s:10:"RefererUrl";s:61:"http://mik-blog.blogspot.com/2005_03_01_mik-blog_archive.html";
                s:8:"FileSize";s:4:"55kB";
                s:10:"FileFormat";s:4:"jpeg";
                s:6:"Height";s:3:"518";
                s:5:"Width";s:3:"700";
                s:9:"Thumbnail";a:3:{
                  s:3:"Url";s:42:"http://sp1.mm-a2.yimg.com/image/2295545420";
                  s:6:"Height";s:3:"111";
                  s:5:"Width";s:3:"150";
                }
              }
            }
          }
        }
        """.lines.map(_.trim).mkString.trim

      val deserialized =
        Php.arr(
          "ResultSet" -> Php.arr(
            "totalResultsAvailable" -> "1177824",
            "totalResultsReturned" -> 2,
            "firstResultPosition" -> PhpInt(1),
            "Result" -> Php.arr(
              0 -> Php.arr(
                "Title" -> "corvette.jpg",
                "Summary" -> "bluefirebar.gif 03-Nov-2003 19:02 22k burning_frax.jpg 05-Jul-2002 14:34 169k corvette.jpg 21-Jan-2004 01:13 101k coupleblack.gif 03-Nov-2003 19:00 3k",
                "Url" -> "http://www.vu.union.edu/~jaquezk/MG/corvette.jpg",
                "ClickUrl" -> "http://www.vu.union.edu/~jaquezk/MG/corvette.jpg",
                "RefererUrl" -> "http://www.vu.union.edu/~jaquezk/MG",
                "FileSize" -> "101.5kB",
                "FileFormat" -> "jpeg",
                "Height" -> "768",
                "Width" -> "1024",
                "Thumbnail" -> Php.arr(
                  "Url" -> "http://sp1.mm-a1.yimg.com/image/2178288556",
                  "Height" -> "120",
                  "Width" -> "160"
                )
              ),
              1 -> Php.arr(
                "Title" -> "corvette_c6_mini_me.jpg",
                "Summary" -> "Corvette I , Corvette II , Diablo , Enzo , Lotus",
                "Url" -> "http://www.ku4you.com/minicars/corvette_c6_mini_me.jpg",
                "ClickUrl" -> "http://www.ku4you.com/minicars/corvette_c6_mini_me.jpg",
                "RefererUrl" -> "http://mik-blog.blogspot.com/2005_03_01_mik-blog_archive.html",
                "FileSize" -> "55kB",
                "FileFormat" -> "jpeg",
                "Height" -> "518",
                "Width" -> "700",
                "Thumbnail" -> Php.arr(
                  "Url" -> "http://sp1.mm-a2.yimg.com/image/2295545420",
                  "Height" -> "111",
                  "Width" -> "150"
                )
              )
            )
          )
        )

      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }

    "handle a data structure containing special characters" in {
      // Adapted from http://stackoverflow.com/questions/2853454/php-unserialize-fails-with-non-encoded-characters

      val serialized = """a:2:{i:0;s:7:"héllö";i:1;s:6:"wörld";}"""

      val deserialized = Php.arr(0 -> "héllö", 1 -> "wörld")

      deserialize(serialized) should equal(deserialized)
      serialize(deserialized) should equal(serialized)
    }
  }
}
