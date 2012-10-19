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

package com.ckkloverdos

package object maybe {
  object UnRecoverableException {
    def unapply(e: Throwable): Option[Error] = {
      require(e ne null, "exception is null")
      e match {
        case e: Error ⇒ Some(e)
        case _ ⇒ None
      }
    }
  }

  object RecoverableException {
    def unapply(e: Throwable): Option[Throwable] = {
      require(e ne null, "exception is null")
      e match {
        case e: Error ⇒ None
        case e ⇒ Some(e)
      }
    }
  }

  def effect[A](f: ⇒ A)(_catch: ⇒ Unit)(_finally: ⇒ Unit): Maybe[A] = {
    try {
      f match {
        case null ⇒ NoVal
        case a    ⇒ Just(a)
      }
    } catch {
      case RecoverableException(e) ⇒
        safeUnit(_catch)
        Failed(e)
    } finally {
      safeUnit(_finally)
    }
  }

  @inline
  def maybe[A](f: ⇒ A) = Maybe(f)

  @inline
  def safeUnit[A](f: ⇒ A): Unit = {
    try f
    catch {
      case RecoverableException(e) ⇒
        ()
    }
  }

  implicit def optionToMaybe[T](x: Option[T]): MaybeOption[T] = x match {
    case Some(c) ⇒ Just(c)
    case None    ⇒ NoVal
  }
  
  implicit def eitherToMaybe[A <: Throwable, B](x: Either[A,  B]): MaybeEither[B] = x match {
    case Left(left)   ⇒ Failed(left)
    case Right(right) ⇒ Just(right)
  }

  def getFromMapAsMaybe[A, B <: AnyRef](map: scala.collection.Map[A, B], key: A): Maybe[B] = Maybe {
   map.get(key) match {
     case Some(value) ⇒
       value

     case None ⇒
       null.asInstanceOf[B]
   }
  }

  def getFromMapAsMaybeOption[A, B <: AnyRef](map: scala.collection.Map[A, B], key: A): MaybeOption[B] = MaybeOption {
   map.get(key) match {
     case Some(value) ⇒
       value

     case None ⇒
       null.asInstanceOf[B]
   }
  }
}
