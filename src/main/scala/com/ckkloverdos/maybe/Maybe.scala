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

import collection.Iterator

/**
 * Inspired by Lift's Box, Haskell's Maybe and Scala's Option.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>.
 */

sealed abstract class Maybe[+A] {
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

  def castTo[B: Manifest]: MaybeOption[B]

  /**
   * Flattens successive maybes once.
   */
  def flatten1[U](implicit ev: A <:< Maybe[U]): Maybe[U]
}

/**
 * A Maybe that can be either a Just or a NoVal. So, this is like Scala's Option
 */
sealed abstract class MaybeOption[+A] extends Maybe[A]

object MaybeOption {
  def apply[A](x: => A): MaybeOption[A] = Maybe(x) match {
    case j@Just(_) => j
    case _         => NoVal
  }
}

/**
 * A maybe that has either failed (`Failed`) or not (`NoVal`).
 *
 * So success is clearly represented by `NoVal`
 */
sealed trait MaybeFailed extends Maybe[Nothing]

object MaybeFailed {
  def apply[A](x: => A): MaybeFailed = Maybe(x) match {
    case f@Failed(_,_) => f
    case _             => NoVal
  }
}

object Maybe {
  val MaybeEmptyIterator   : Iterator   [Nothing] = Iterator   ()
  val MaybeEmptyTraversable: Traversable[Nothing] = Traversable()
  val MaybeEmptyList       : List       [Nothing] = List       ()
  val MaybeEmptySet        : Set        [Nothing] = Set        ()

  implicit def optionToMaybe[T](x: Option[T]) = x match {
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

final case class Just[@specialized +A](get: A) extends MaybeOption[A] {
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
  
  def castTo[B: Manifest] = get match {
    case null  => NoVal // normally null should not even be here but we are being cautious
    case value => if(manifest[B].erasure.isInstance(value)) this.asInstanceOf[MaybeOption[B]] else NoVal
  }

  def flatten1[U](implicit ev: A <:< Maybe[U]): Maybe[U] = ev(get)

  override def equals(that: Any) =
    that.isInstanceOf[Just[_]] && that.asInstanceOf[Just[_]].get == this.get
}

case object NoVal extends MaybeOption[Nothing] with MaybeFailed {
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
  
  def castTo[B: Manifest]: MaybeOption[B] = NoVal

  def flatten1[U](implicit ev: <:<[Nothing, Maybe[U]]) = this

  override def equals(that: Any) = that.asInstanceOf[AnyRef] eq NoVal
}

/**
 * A Maybe wrapper for an exception.
 */
final case class Failed(exception: Throwable, explanation: String = "") extends Maybe[Nothing] with MaybeFailed {
  def this(exception: Throwable, fmt: String, args: Any*) = this(exception, fmt.format(args))
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
  
  def castTo[B: Manifest]: MaybeOption[B] = NoVal

  def flatten1[U](implicit ev: <:<[Nothing, Maybe[U]]) = this

  override def equals(that: Any) =
    that.isInstanceOf[Failed] &&
      that.asInstanceOf[Failed].exception   == this.exception &&
      that.asInstanceOf[Failed].explanation == this.explanation
}
