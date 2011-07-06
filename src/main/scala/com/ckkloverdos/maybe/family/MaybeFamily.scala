package com.ckkloverdos.maybe.family


trait MaybeFamily {
  type AnyMaybe  = MaybeFamily#Maybe[_]
  type AnyJust   = MaybeFamily#Just[_]
  type AnyNoVal  = MaybeFamily#NoVal
  type AnyFailed = MaybeFamily#Failed[_]
  
  val MaybeEmptyIterator: Iterator[Nothing] = Iterator()
  val MaybeEmptyTraversable: Traversable[Nothing] = Traversable()
  val MaybeEmptyList: List[Nothing] = List()
  val MaybeEmptySet: Set[Nothing] = Set()

  implicit def optionToMaybe[T](x: Option[T]): Maybe[T] = x match {
    case Some(c) => createJust(c)
    case None => createNoVal
  }

  def createJust[A](a: A): Just[A]
  def createNoVal: NoVal
  def createFailed[T: Manifest](message: StdMaybe.Maybe[String], cause: StdMaybe.Maybe[Throwable], data: StdMaybe.Maybe[T]): Failed[T]

  @inline
  final def createMaybe[A](x: => A): Maybe[A] = apply(x)
  /**
   * This is a polymorphic constructor for Maybes within this family.
   * Use it if you are not sure what to expect from `x`.
   */
  final def apply[A](x: => A): Maybe[A] = {
    try {
      val value = x
      value match {
        case null => createNoVal
        case _ => createJust(value)
      }
    } catch {
      case e: Throwable => createFailed(
        StdMaybe.Just("Maybe() failed"),
        StdMaybe.Just(e),
        StdMaybe.NoVal)
    }
  }

  trait Maybe[+A] extends Equals {
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

    def fold[T](onJust: => T, onNil: => T, onFailed: => T): T

    @inline
    final def forJust[B >: A](f: Just[A] => Maybe[B]): Maybe[B] =
      if(isJust) f(this.asInstanceOf[Just[A]]) else this

    @inline
    final def forNoVal[B >: A](f: => Maybe[B]): Maybe[B] =
      if(isNoVal) f else this

    @inline
    final def forFailed[B >: A, T, S](f: Failed[T] => Maybe[B]): Maybe[B] =
      if(isFailed) f(this.asInstanceOf[Failed[T]]) else this

    def canEqual(that: Any): Boolean = that match {
      case _: Maybe[_] => true
      case _ => false
    }

    override def equals(that: Any) = that match {
      case b: Maybe[_] if b.canEqual(this) => equalsImpl(b)
      case _ => false
    }

    protected def equalsImpl(that: Maybe[_]): Boolean
  }

  abstract class Just[@specialized(Boolean, Char, Int, Double) +A](val get: A) extends Maybe[A] {
    def toIterator = Iterator(get)

    def toTraversable = Traversable(get)

    def toOption = Some(get)

    def toList = List(get)

    def isJust = true

    def isFailed = false

    def isNoVal = false

    def getOr[B >: A](b: => B) = get

    def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
      onJust

    def ||[B >: A](f: => Maybe[B]) = this

    def map[B](f: (A) => B) = createJust(f(get))

    def flatMap[B](f: (A) => Maybe[B]) = f(get)

    def filter(f: (A) => Boolean): Maybe[A] = if(f(get)) this else createNoVal

    def foreach(f: A => Unit) = f(get)

    protected def equalsImpl(that: Maybe[_]) =
      that.isJust && that.asInstanceOf[Just[A]].get == get
  }

  abstract class NoVal extends Maybe[Nothing] {
    def toIterator = MaybeEmptyIterator

    def toTraversable = MaybeEmptyTraversable

    def toOption = None

    def toList = MaybeEmptyList

    def isJust = false

    def isNoVal = true

    def isFailed = false

    def getOr[B >: Nothing](b: => B) = b

    def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
      onNil

    def ||[B >: Nothing](f: => Maybe[B]) = f

    def map[B](f: (Nothing) => B) = createNoVal

    def flatMap[B](f: (Nothing) => Maybe[B]) = createNoVal

    def filter(f: (Nothing) => Boolean) = createNoVal

    def foreach(f: Nothing => Unit) = {}

    protected def equalsImpl(that: Maybe[_]) =
      that.isNoVal
  }

  abstract class Failed[T: Manifest](val message  : Maybe[String],
                                     val cause    : Maybe[Throwable],
                                     val data     : Maybe[T]) extends Maybe[Nothing] {

    def typeOfData = manifest[T]

    def isJust = false

    def isNoVal = false

    def isFailed = true

    def toIterator = MaybeEmptyIterator

    def toTraversable = MaybeEmptyTraversable

    def toOption = None

    def toList = MaybeEmptyList

    def toSet = MaybeEmptySet

    def getOr[B >: Nothing](b: => B) = b

    def fold[T](onJust: => T, onNil: => T, onFailed: => T): T =
      onFailed

    def ||[B >: Nothing](f: => Maybe[B]) = f

    def map[B](f: (Nothing) => B) = this

    def flatMap[B](f: (Nothing) => Maybe[B]) = this

    def filter(f: (Nothing) => Boolean) = this

    def foreach(f: Nothing => Unit) = {}

    protected def equalsImpl(that: Maybe[_]) = {
      that.isFailed && {
        val tf = that.asInstanceOf[Failed[_]]
        tf.message == this.message &&
        tf.cause   == this.cause   &&
        tf.data    == this.data
      }
    }
  }
}

final object StdMaybe extends MaybeFamily {
  def createJust[A](a: A) = Just(a)

  def createNoVal = NoVal

  def createFailed[T: Manifest](message: Maybe[String], cause: Maybe[Throwable], data: Maybe[T]): Failed[T] =
    Failed(message, cause, data)


  sealed trait Maybe[+A] extends super.Maybe[A]

  final case class Just[+A](override val get: A) extends super.Just(get) with Maybe[A]
  final case object NoVal extends super.NoVal with Maybe[Nothing]
  final case class Failed[T: Manifest](override val message  : Maybe[String],
                                       override val cause    : Maybe[Throwable],
                                       override val data     : Maybe[T]) extends super.Failed[T](message, cause, data) with Maybe[Nothing]
}





