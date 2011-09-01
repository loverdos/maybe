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
  def isNoVal: Boolean
  def isFailed: Boolean

  def getOr[B >: A](b: => B): B

  def ||[B >: A](f: => Maybe[B]): Maybe[B]

  def map[B](f: A => B): Maybe[B]

  def flatMap[B](f: A => Maybe[B]): Maybe[B]

  @inline
  final def >>[B](f: A => Maybe[B]): Maybe[B] = this.flatMap(f)

  def filter(f: A => Boolean): Maybe[A]

  def foreach(f: A => Unit): Unit

  def fold[T](onJust: (A) => T)(onNoVal: => T)(onFailed: (Failed) => T): T

  @inline
  final def fold_[T](onJust: => T)(onNoVal: => T)(onFailed: => T): T =
    fold(a => onJust)(onNoVal)(f => onFailed)

  @inline
  final def foldJust[T](onJust: (A) => T)(onOther: => T): T =
    fold(onJust)(onOther)(f => onOther)

  @inline
  final def forJust[B >: A](f: Just[A] => Maybe[B]): Maybe[B] =
    if(isJust) f(this.asInstanceOf[Just[A]]) else this

  @inline
  final def forNoVal[B >: A](f: => Maybe[B]): Maybe[B] =
    if(isNoVal) f else this

  @inline
  final def forFailed[B >: A](f: Failed => Maybe[B]): Maybe[B] =
    if(isFailed) f(this.asInstanceOf[Failed]) else this

  def canEqual(that: Any): Boolean = that match {
    case _: Maybe[_] => true
    case _           => false
  }

  override def equals(that: Any) = that match {
    case b: Maybe[_] if b.canEqual(this) => equalsImpl(b)
    case _ => false
  }

  protected def equalsImpl(that: Maybe[_]): Boolean
}

/**
 * A Maybe that can be either a Just or a NoVal. So, this is like Scala's Option
 */
sealed abstract class MaybeOption[+A] extends Maybe[A] {
  override def canEqual(that: Any): Boolean = that match {
    case _: MaybeOption[_] => true
    case _                 => false
  }
}

object MaybeOption {
  def apply[A](x: => A): MaybeOption[A] = Maybe(x) match {
    case j@Just(_) => j
    case _         => NoVal
  }
}

object Maybe {
  val MaybeEmptyIterator   : Iterator   [Nothing] = Iterator   ()
  val MaybeEmptyTraversable: Traversable[Nothing] = Traversable()
  val MaybeEmptyList       : List       [Nothing] = List       ()
  val MaybeEmptySet        : Set        [Nothing] = Set        ()

  implicit def optionToBox[T](x: Option[T]) = x match {
    case Some(c) => Just(c)
    case None    => NoVal
  }

  /**
   * This is a polymorphic constructor for Maybes.
   * Use it if you are not sure what to expect from `x`.
   */
  def apply[A](x: => A): Maybe[A] = {
    try {
      val value = x
      value match {
        case null => NoVal
        case _    => Just(value)
      }
    } catch {
      case e: Throwable => Failed(e, "Maybe.apply()")
    }
  }
}

final case class Just[@specialized(Boolean, Char, Int, Double) +A](get: A) extends MaybeOption[A] {
  def toIterator    = Iterator(get)
  def toTraversable = Traversable(get)
  def toOption      = Some(get)
  def toList        = List(get)

  def isJust = true
  def isFailed = false
  def isNoVal = false

  def getOr[B >: A](b: => B) = get

  def fold[T](onJust: (A) => T)(onOnVal: => T)(onFailed: (Failed) => T) = onJust(get)

  def ||[B >: A](f: => Maybe[B]) = this

  def map[B](f: (A) => B)= Just(f(get))
  def flatMap[B](f: (A) => Maybe[B]) = f(get)
  def filter(f: (A) => Boolean): Maybe[A] = if(f(get)) this else NoVal
  def foreach(f: A => Unit) = f(get)

  protected def equalsImpl(that: Maybe[_]) = that match {
    case Just(v) => v == get
    case _       => false
  }
}

case object NoVal extends MaybeOption[Nothing] {
  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList

  def isJust   = false
  def isNoVal  = true
  def isFailed = false

  def getOr[B >: Nothing](b: => B) = b

  def fold[T](onJust: (Nothing) => T)(onNoVal: => T)(onFailed: (Failed)=> T) = onNoVal

  def ||[B >: Nothing](f: => Maybe[B]) = f

  def map[B](f: (Nothing) => B)= NoVal
  def flatMap[B](f: (Nothing) => Maybe[B]) = NoVal
  def filter(f: (Nothing) => Boolean) = NoVal
  def foreach(f: Nothing => Unit) = {}

  protected def equalsImpl(that: Maybe[_]) = that match {
    case NoVal => true
    case _     => false
  }
}

/**
 * A Maybe wrapper for an exception.
 */
final case class Failed private[maybe](exception: Throwable, explanation: String) extends Maybe[Nothing] {

  def isJust   = false
  def isNoVal  = false
  def isFailed = true

  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList
  def toSet         = Maybe.MaybeEmptySet

  def getOr[B >: Nothing](b: => B) = b

  def fold[T](onJust: (Nothing) => T)(onNoVal: => T)(onFailed: (Failed)=> T) = onFailed(this)

  def ||[B >: Nothing](f: => Maybe[B]) = f

  def map[B](f: (Nothing) => B) = this
  def flatMap[B](f: (Nothing) => Maybe[B]) = this
  def filter(f: (Nothing) => Boolean) = this
  def foreach(f: Nothing => Unit) = {}

  protected def equalsImpl(that: Maybe[_]) = that match {
    case Failed(eO, ex0) => eO == exception && ex0 == explanation
    case _ => false
  }
}

object Failed {
  def apply(exception: Throwable): Failed = Failed(exception)
  def apply(exception: Throwable, explanation: String, explanationArgs: Any*): Failed =
    Failed(exception, explanation.format(explanationArgs))
}