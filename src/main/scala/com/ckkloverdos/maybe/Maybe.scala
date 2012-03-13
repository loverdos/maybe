/*
 * Copyright 2011-2012 Christos KK Loverdos
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
 * Inspired by Lift's `Box`, Haskell's `Maybe` and Scala's `Option`.
 *
 * @author Christos KK Loverdos <loverdos@gmail.com>
 */
sealed abstract class Maybe[+A] extends Serializable {
  def toIterator: Iterator[A]
  def toTraversable: Traversable[A]
  def toOption: Option[A]
  def toList: List[A]

  def isJust: Boolean
  def isNoVal: Boolean
  def isFailed: Boolean

  def getOr[B >: A](b: ⇒ B): B

  def ||[B >: A](f: ⇒ Maybe[B]): Maybe[B]

  def map[B](f: A ⇒ B): Maybe[B]

  def flatMap[B](f: A ⇒ Maybe[B]): Maybe[B]

  @inline
  final def mapJust[B >: A](f: Just[A] ⇒ Maybe[B]): Maybe[B] =
    if(isJust) f(this.asInstanceOf[Just[A]]) else this

  @inline
  final def mapNoVal[B >: A](f: ⇒ Maybe[B]): Maybe[B] =
    if(isNoVal) f else this

  @inline
  final def mapFailed[B >: A](f: Failed ⇒ Maybe[B]): Maybe[B] =
    if(isFailed) f(this.asInstanceOf[Failed]) else this

  /**
   * Map or return the provided default value.
   */
  @inline
  def defaultMap[B](default: ⇒ B)(f: A ⇒ B): B = map(f) getOr default

  /**
   * Use this value of type `A` to provide another one of type `B` and then
   * do some cleanup on the original value of type `A`.
   *
   * Use case: Get a DB cursor, perform calculations based on that and then close the cursor.
   */
  def finallyMap[B](_finally: A ⇒ Unit)(f: A ⇒ B): Maybe[B]
  
  def finallyFlatMap[B](_finally: A ⇒ Unit)(f: A ⇒ Maybe[B]): Maybe[B]

  @inline
  final def >>[B](f: A ⇒ Maybe[B]): Maybe[B] = this flatMap f

  def filter(f: A ⇒ Boolean): Maybe[A]

  def foreach(f: A ⇒ Unit): Unit

  def fold[T](onJust: (A) ⇒ T)(onNoVal: ⇒ T)(onFailed: (Failed) ⇒ T): T

  @inline
  final def foldUnit(onJust: ⇒ Any)(onNoVal: ⇒ Any)(onFailed: ⇒ Any): Unit =
    fold(a ⇒ onJust)(onNoVal)(f ⇒ onFailed)

  @inline
  final def foldJust[T](onJust: (A) ⇒ T)(onOther: ⇒ T): T =
    fold(onJust)(onOther)(f ⇒ onOther)

  def castTo[B <: AnyRef : Manifest]: Maybe[B]

  /**
   * Flattens two successive maybes to one.
   */
  def flatten1[U](implicit ev: A <:< Maybe[U]): Maybe[U]
}

/**
 * A Maybe that can be either a Just or a NoVal. So, this is like Scala's Option.
 */
sealed trait  MaybeOption[+A] extends Maybe[A]

object MaybeOption {
  def apply[A](x: ⇒ A): MaybeOption[A] = Maybe(x) match {
    case j@Just(_) ⇒ j
    case _         ⇒ NoVal
  }
}

/**
 * A Maybe that can be either a Just or a Failed. So, this is like Scala's Either.
 */
sealed trait MaybeEither[+A] extends Maybe[A]

object MaybeEither {
  def apply[A](x: ⇒ A): MaybeEither[A] = Maybe(x) match {
    case j@Just(_) ⇒
      j
    case f@Failed(_) ⇒
      f
    case NoVal ⇒
      Failed(new Exception("Got NoVal for a MaybeFailed"))
  }
}

object Maybe {
  private[maybe] final val MaybeEmptyIterator   : Iterator   [Nothing] = Iterator   ()
  private[maybe] final val MaybeEmptyTraversable: Traversable[Nothing] = Traversable()
  private[maybe] final val MaybeEmptyList       : List       [Nothing] = List       ()
  private[maybe] final val MaybeEmptySet        : Set        [Nothing] = Set        ()

  /**
   * This is a polymorphic constructor for Maybes.
   * Use it if you are not sure what to expect from `x`.
   */
  def apply[A](x: ⇒ A): Maybe[A] = {
    try {
      val value = x
      value match {
        case null ⇒ NoVal
        case _    ⇒ Just(value)
      }
    } catch {
      case e: Throwable ⇒ Failed(e)
    }
  }
}

final case class Just[+A](get: A) extends MaybeOption[A] with MaybeEither[A] {
  def toIterator    = Iterator(get)
  def toTraversable = Traversable(get)
  def toOption      = Some(get)
  def toList        = List(get)

  def isJust = true
  def isFailed = false
  def isNoVal = false

  def getOr[B >: A](b: ⇒ B) = get


  def finallyMap[B](_finally: A ⇒ Unit)(f: A ⇒ B): Maybe[B] = {
    try this.map(f)
    finally { safeUnit(_finally(get)) }
  }

  def finallyFlatMap[B](_finally: A ⇒ Unit)(f: A ⇒ Maybe[B]): Maybe[B] = {
    try this.flatMap(f)
    finally { safeUnit(_finally(get)) }
  }

  def fold[T](onJust: (A) ⇒ T)(onOnVal: ⇒ T)(onFailed: (Failed) ⇒ T) = onJust(get)

  def ||[B >: A](f: ⇒ Maybe[B]) = this

  def map[B](f: (A) ⇒ B)= Maybe(f(get))
  def flatMap[B](f: (A) ⇒ Maybe[B]) = {
    try f(get)
    catch {
      case t: Throwable ⇒ Failed(t)
    }
  }
  def filter(f: (A) ⇒ Boolean): Maybe[A] = {
    try {
      if(f(get)) this else NoVal
    } catch {
      case t: Throwable ⇒ Failed(t)
    }
  }
  def foreach(f: A ⇒ Unit) = f(get)

  def castTo[B <: AnyRef : Manifest] = get match {
    case null ⇒ NoVal
    case value if(manifest[B].erasure.isInstance(value)) ⇒ this.asInstanceOf[Maybe[B]]
    case value ⇒ Failed(
      new ClassCastException("%s -> %s".format(
        get.asInstanceOf[AnyRef].getClass.getName,
        manifest[B].erasure.getName)))
  }

  def flatten1[U](implicit ev: A <:< Maybe[U]): Maybe[U] = ev(get)

  override def equals(that: Any) =
    that.isInstanceOf[Just[_]] && that.asInstanceOf[Just[_]].get == this.get
}

case object NoVal extends MaybeOption[Nothing] {
  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList

  def isJust   = false
  def isNoVal  = true
  def isFailed = false

  def getOr[B >: Nothing](b: ⇒ B) = b

  def fold[T](onJust: (Nothing) ⇒ T)(onNoVal: ⇒ T)(onFailed: (Failed)⇒ T) = onNoVal

  def ||[B >: Nothing](f: ⇒ Maybe[B]) = f

  def map[B](f: (Nothing) ⇒ B)= NoVal
  def flatMap[B](f: (Nothing) ⇒ Maybe[B]) = NoVal
  def filter(f: (Nothing) ⇒ Boolean) = NoVal
  def foreach(f: Nothing ⇒ Unit) = {}

  def finallyMap[B](_finally: (Nothing) ⇒ Unit)(f: (Nothing) ⇒ B) = this

  def finallyFlatMap[B](_finally: (Nothing) ⇒ Unit)(f: (Nothing) ⇒ Maybe[B]) = this

  def castTo[B <: AnyRef : Manifest] = this

  def flatten1[U](implicit ev: <:<[Nothing, Maybe[U]]) = this

  override def equals(that: Any) = that.asInstanceOf[AnyRef] eq NoVal
}

/**
 * A Maybe wrapper for an exception.
 */
final case class Failed(exception: Throwable) extends MaybeEither[Nothing] {
  require(exception ne null, "exception is null")

  def isJust   = false
  def isNoVal  = false
  def isFailed = true

  def toIterator    = Maybe.MaybeEmptyIterator
  def toTraversable = Maybe.MaybeEmptyTraversable
  def toOption      = None
  def toList        = Maybe.MaybeEmptyList
  def toSet         = Maybe.MaybeEmptySet

  def getOr[B >: Nothing](b: ⇒ B) = b

  def fold[T](onJust: (Nothing) ⇒ T)(onNoVal: ⇒ T)(onFailed: (Failed)⇒ T) = onFailed(this)

  def ||[B >: Nothing](f: ⇒ Maybe[B]) = f

  def map[B](f: (Nothing) ⇒ B) = this
  def flatMap[B](f: (Nothing) ⇒ Maybe[B]) = this
  def filter(f: (Nothing) ⇒ Boolean) = this
  def foreach(f: Nothing ⇒ Unit) = {}

  def finallyMap[B](_finally: (Nothing) ⇒ Unit)(f: (Nothing) ⇒ B) = this

  def finallyFlatMap[B](_finally: (Nothing) ⇒ Unit)(f: (Nothing) ⇒ Maybe[B]) = this

  def castTo[B <: AnyRef : Manifest] = this

  def flatten1[U](implicit ev: <:<[Nothing, Maybe[U]]) = this

  override def equals(that: Any) = {
    that match {
      case failed: Failed ⇒
        this.exception  == failed.exception
      case _ ⇒
        false
    }
  }
}