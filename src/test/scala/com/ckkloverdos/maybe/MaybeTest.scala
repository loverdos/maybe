/*
 * Copyright 2011-2011 Christos KK Loverdos
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ckkloverdos.maybe

import org.junit.Assert
import org.junit.Test

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */

class MaybeTest {
  val items = List(
    1,
    "Hello world",
    (x: Int) => x * x,
    new java.lang.Double(2.0),
    List(1, 2, 3))
  
  @Test
  def testNullToNoVal = {
    Assert.assertTrue(Maybe(null) == NoVal)
  }

  @Test
  def testNotNullToJust = {
    for(item <- items) {
      Assert.assertTrue(Maybe(item).isJust)
    }
  }

  @Test
  def testExceptionToFailed = {
    Assert.assertTrue(Maybe(throw new Exception).isFailed)
  }

  @Test
  def testMatchJust = {
    val Hello = "Hello"
    Just(Hello) match {
      case Just(Hello) => ()
      case _ => Assert.assertFalse(true)
    }
  }

  @Test
  def testMatchFailed = {
    val Except = new Exception("Hello")
    val Explan = "Hello there"
    Failed(Except, Explan) match {
      case Failed(Except, Explan) => ()
      case _ => Assert.assertFalse(true)
    }
  }

  @Test
  def testMatchNoVal = {
    NoVal match {
      case NoVal => ()
      case _ => Assert.assertFalse(true)
    }
  }

  @Test
  def testEqJust {
    for(item <- items) {
      Assert.assertEquals(Just(item), Just(item))
    }
  }

  @Test
  def testEqNoVal {
    Assert.assertEquals(NoVal, NoVal)
  }

  @Test
  def testFlatten1 {
    Assert.assertEquals(Maybe("foo"), Maybe(Maybe("foo")).flatten1)
  }

  @Test
  def testCastNullToNoVal {
    Assert.assertEquals(NoVal, Just(null).castTo[Int])
  }

  @Test
  def testCastTo {
    val help = "Help"
    val aJust: Maybe[_] = Just(help)
    Assert.assertTrue(aJust.castTo[CharSequence].isJust)
  }
}
