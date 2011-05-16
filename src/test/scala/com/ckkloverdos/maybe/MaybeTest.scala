/*
 * Copyright 2010-2011 Christos KK Loverdos
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ckkloverdos.maybe

import org.scalatest.FlatSpec

/**
 * 
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */
class MaybeTest extends FlatSpec {

  behavior of "Maybe"

  it should "translate null to Nil" in {
    assert(Maybe(null) === Nil)
  }

  it should "translate a non-null value x to Just(x)" in {
    val items = List(
    1,
    "Hello world",
    (x: Int) => x * x,
    new java.lang.Double(2.0),
    List(1, 2, 3)
    )

    for(item <- items) {
      assert(Maybe(item) === Just(item))
    }
  }

  it should "translate a body that throws an exception to a Failed()" in {
    assert(Maybe(throw new Exception).isFailed)
  }
}
