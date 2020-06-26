package net.degoes

/*
 * INTRODUCTION
 *
 * In Functional Design, composable operators allow building infinitely many
 * solutions from a few operators and domain constructors.
 *
 * Operators and constructors are either primitive, meaning they cannot be
 * expressed in terms of others, or they are derived, meaning they can be§
 * expressed in terms of other operators or constructors.
 *
 * The choice of primitives determine how powerful and expressive a domain
 * model is. Some choices lead to weaker models, and others, to more powerful
 * models. Power is not always a good thing: constraining the power of a model
 * allows more efficient and more feature-full execution.
 *
 * Derived operators and constructors bridge the gap from the domain, to common
 * problems that a user of the domain has to solve, improving productivity.
 *
 * In many domains, there exist many potential choices for the set of primitive
 * operators and constructors. But not all choices are equally good.
 *
 * The best primitives are:
 *
 * * Composable, to permit a lot of power in a small, reasonable package
 * * Expressive, to solve the full range of problems in the domain
 * * Orthogonal, such that no primitive provides the capabilities of any other
 *
 * Orthogonality also implies minimalism, which means the primitives are the
 * smallest set of orthogonal primitives that exist.ww
 * 
 * derived operators are anyway super useful, e.g. repeat in  a parser
 * => primitive constr + op == power
 *    derived               == convenience
 * 
 * Orthogonality goes hand-in-hand with modularity, because if every component deals with only 1 problem
 *
 * If we take the Quiz domain, if we delete + we still have composability because we can make bigger solutions out of small blocks
 * Orthogonality: We want our primitive to deal with one concern and only one
 */

/**
 * ORTHOGONALITY - EXERCISE SET 1
 */
object email_filter3 {

  final case class Address(emailAddress: String)
  final case class Email(sender: Address, to: List[Address], subject: String, body: String)

  /**
   * EXERCISE 1
   *
   * In declarative encoding the amount of work I do is proportional to the amount of terms in my sum type
   * Therefore, we want to chop some constructors off, so better derive from a small set
   * In the following model, which describes an email filter, there are many
   * primitives with overlapping responsibilities. Find the smallest possible
   * set of primitive operators and constructors, without deleting any
   * constructors or operators (you may implement them in terms of primitives).
   *
   * NOTE: You may *not* use a final encoding, which would allow you to
   * collapse everything down to one primitive.
   */
  sealed trait EmailFilter { self =>
    def &&(that: EmailFilter): EmailFilter = EmailFilter.And(self, that)

    def ||(that: EmailFilter): EmailFilter = (self.negate && that.negate).negate

    def ^^(that: EmailFilter): EmailFilter = (self || that) && (self && that).negate

    def negate: EmailFilter = EmailFilter.Not(self)
  }
  object EmailFilter {

    final case class And(left: EmailFilter, right: EmailFilter)         extends EmailFilter
    final case class Not(value: EmailFilter)                            extends EmailFilter 
    final case class SenderIn(targets: Set[Address])                    extends EmailFilter
    final case class RecipientIn(targets: Set[Address])                 extends EmailFilter
    final case class BodyContains(phrase: String)                       extends EmailFilter
    final case class SubjectContains(phrase: String)                    extends EmailFilter

    val always: EmailFilter = senderIn(Set()).negate

    val never: EmailFilter = always.negate

    def senderIs(sender: Address): EmailFilter = senderIn(Set(sender))

    def senderIsNot(sender: Address): EmailFilter = senderIs(sender).negate

    def recipientIs(recipient: Address): EmailFilter = recipientIn(Set(recipient))

    def recipientIsNot(recipient: Address): EmailFilter = recipientIs(recipient).negate

    def senderIn(senders: Set[Address]): EmailFilter = SenderIn(senders)

    def recipientIn(recipients: Set[Address]): EmailFilter = RecipientIn(recipients)

    def bodyContains(phrase: String): EmailFilter = BodyContains(phrase)

    def bodyDoesNotContain(phrase: String): EmailFilter = BodyContains(phrase).negate

    def subjectContains(phrase: String): EmailFilter = SubjectContains(phrase)

    def subjectDoesNotContain(phrase: String): EmailFilter = SubjectContains(phrase).negate
  }
}

/**
 * COMPOSABILITY - EXERCISE SET 2
 */
object ui_components {

  /**
   * EXERCISE 1
   *
   * The following API is not composable—there is no domain. Introduce a
   * domain with elements, constructors, and composable operators.
   */

   sealed trait Turtle2 { self =>
      def +(next: Turtle2): Turtle2 = Turtle2.Sequence(self, next)

   }
   object Turtle2 {

     final case class TurnLeft(degrees: Int) extends Turtle2
     final case object GoForward extends Turtle2
     final case class Sequence(first: Turtle2, second: Turtle2) extends Turtle2
     case object Draw extends Turtle2

     def turnLeft(degrees: Int): Turtle2 = TurnLeft(degrees)
     def turnRight(degrees: Int): Turtle2 = turnLeft(360 - degrees) 
     def goForward(): Turtle2 = GoForward
     def goBackward(): Turtle2 = turnLeft(180) + goForward() + turnLeft(180)
     def standStill: Turtle2 = TurnLeft(0)
     def repeat(n: Int, move: Turtle2): Turtle2 = List.fill(n)(move).foldLeft()
     def draw: Move = Draw

   }

   final case class ExecTurtle(run: Turtle2 => Unit) {self =>
      def + (that: ExecTurtle): ExecTurtle = ExecTurtle {turtle =>
        self.run(turtle)
        that.run(turtle)
      }
   } 

   object ExecTurtle {
     def turnLeft(degrees: Int): ExecTurtle = ExecTurtle(_.turnLeft(degrees))
     def turnRight(degrees: Int): ExecTurtle = ExecTurtle(_.turnLeft(-degrees))
   }
  trait Turtle { self =>
    def turnLeft(degrees: Int): Unit

    def turnRight(degrees: Int): Unit

    def goForward(): Unit

    def goBackward(): Unit

    def draw(): Unit
  }
}
