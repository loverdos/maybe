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

import collection.Iterator

/**
 * Inspired by Lift's Box, Haskell's Maybe and Scala's Option.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */

sealed abstract class Maybe[+A] extends Equals {
  def toIterator: Iterator[A]
  def toTraversable: Traversable[A]
  def toOption: Option[A]
  def toList: List[A]

  def isJust: Boolean
  def isNil: Boolean
  def isFailed: Boolean

  def getOr[B >: A](b: => B): B

  def ||[B >: A](f: => Maybe[B]): Maybe[B]

  def map[B](f: A => B): Maybe[B]

  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  @inline
  final def >>[B](f: A => Maybe[B]): Maybe[B] = this.flatMap(f)

  def filter(f: A => Boolean): Maybe[A]

  def foreach(f: A => Unit): Unit

  def fold[T](onJust: => T, onNil: => T, onFailed: => T): T

  @inline
  final def forJust[B >: A](f: Just[A] => Maybe[B]): Maybe[B] =
    if(isJust) f(this.asInstanceOf[Just[A]]) else this

  @inline
  final def forNil[B >: A](f: => Maybe[B]): Maybe[B] =
    if(isNil) f else this

  @inline
  final def forFailed[B >: A, T, S](f: Failed[T] => Maybe[B]): Maybe[B] =
    if(isFailed) f(this.asInstanceOf[Failed[T]]) else this

  def canEqual(that: Any): Boolean = that match {
    case _: Maybe[_] => true
    case _         => false
  }

  override def equals(that: Any) = that match {
    case b: Maybe[_] if b.canEqual(this) => equalsImpl(b)
    case _ => false
  }

  protected def equalsImpl(that: Maybe[_]): Boolean
}

object Maybe {
  type AnyFailure = Failed[_]
  val MaybeEmptyIterator   : Iterator   [Nothing] = Iterator   ()
  val MaybeEmptyTraversable: Traversable[Nothing] = Traversable()
  val MaybeEmptyList       : List       [Nothing] = List       ()
  val MaybeEmptySet        : Set        [Nothing] = Set        ()

  implicit def optionToBox[T](x: Option[T]) = x match {
    case Some(c) => Just(c)
    case None    => Nil
  }

  /**
   * This is a polymorphic constructor for Maybes.
   * Use it if you are not sure what to expect.
   */
  def apply[A](x: => A) = {
    try {
      val value = x
      value match {
        case null => Nil
        case _    => Just(value)
      }
    } catch {
      case e: Throwable => Failed(Some("Maybe() failed"), Just(e), Nil, Nil)
    }
  }
}

//object Just {
//  def apply[A](x: => A): Maybe[A] = Maybe(x)
//
//  def unapply[A](x: A): Option[A] = x match {
//    case null => None
//    case _    => Some(x)
//  }
//}

final case class Just[@specialized(Boolean, Char, Int, Double) +A](get: A) extends Maybe[A] {
  def toIterator    = Iterator(get)
  def toTraversable = Traversable(get)
  def toOption      = Some(get)
  def toList        = List(get)

  def isJust = true
  def isFailed = false
  def isNil = false

  def getOr[B >: A](b: => B) = get

  def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
    onJust

  def ||[B >: A](f: => Maybe[B]) = this

  def map[B](f: (A) => B)= Just(f(get))
  def flatMap[B](f: (A) => Maybe[B]) = f(get)
  def filter(f: (A) => Boolean): Maybe[A] = if(f(get)) this else Nil
  def foreach(f: A => Unit) = f(get)

  protected def equalsImpl(that: Maybe[_]) = that match {
    case Just(v) => v == get
    case _         => false
  }
}

case object Nil extends Maybe[Nothing] {
  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList

  def isJust= false
  def isNil = true
  def isFailed= false

  def getOr[B >: Nothing](b: => B) = b

  def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
    onNil

  def ||[B >: Nothing](f: => Maybe[B]) = f

  def map[B](f: (Nothing) => B)= Nil
  def flatMap[B](f: (Nothing) => Maybe[B]) = Nil
  def filter(f: (Nothing) => Boolean) = Nil
  def foreach(f: Nothing => Unit) = {}

  protected def equalsImpl(that: Maybe[_]) = that match {
    case Nil => true
    case _     => false
  }
}

final case class Failed[T: Manifest](
  message: Maybe[String],
  exception: Maybe[Throwable],
  cause: Maybe[Failed[_]],
  data: Maybe[T]
  ) extends Maybe[Nothing] {

  def typeOfData = manifest[T]

  def isJust   = false
  def isNil    = false
  def isFailed = true

  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList
  def toSet         = Maybe.MaybeEmptySet

  def getOr[B >: Nothing](b: => B) = b

  def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
    onFailed

  def ||[B >: Nothing](f: => Maybe[B]) = f

  def map[B](f: (Nothing) => B) = this
  def flatMap[B](f: (Nothing) => Maybe[B]) = this
  def filter(f: (Nothing) => Boolean) = this
  def foreach(f: Nothing => Unit) = {}

  protected def equalsImpl(that: Maybe[_]) = that match {
    case Failed(mO, eO, cO, dB) =>
      mO == message &&
      eO == exception &&
      cO == cause &&
      dB == data
    case _ => false
  }
}

