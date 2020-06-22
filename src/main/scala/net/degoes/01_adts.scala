package net.degoes

import java.time.Instant
import java.time.YearMonth
import java.time.Duration

/*
 * INTRODUCTION
 *
 * Functional Design depends heavily on functional data modeling. Functional
 * data modeling is the task of creating precise, type-safe models of a given
 * domain using algebraic data types and generalized algebraic data types.
 *
 * In this section, you'll review basic functional domain modeling.
 */

/**
 * E-COMMERCE - EXERCISE SET 1
 *
 * Consider an e-commerce application that allows users to purchase products.
 */

object ATD { // a data type made of sums and products, recursively
  
  //product type
  final case class Person(name: String, age: Int) //  String * Int (cartesian product)

  //sum type
  sealed trait Color
  object Color {
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color
    final case class Custom(red: Int, green: Int, blue: Int) extends Color 
    final case class CustomPolymorphic[A](red: A, green: A, blue: A) extends Color  //this is a family of colors that can work with Int, Double etc
  }
}
 object credit_card {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a credit card, which must have:
   *
   *  * Number
   *  * Name
   *  * Expiration date
   *  * Security code
   */
  // v1: number can't have leading zeroes, security code can be negative, name can be empty etc
  // But sometimes it can be painful to create an ADT for everything. A smart constructor is most of the time enough
  // final case class CreditCard(number: BigInt, name: String, expDate: YearMonth, securityCode: Int) 
  final case class CreditCard(number: BigInt, name: Name, expDate: YearMonth, securityCode: Int) 

  sealed abstract case class Name private (name: String)
  object Name {
    // --> SMART CONSTRUCTOR <--
    def fromString(name: String): Option[Name] = 
      if (name.trim.length == 0) None
      else Some(new Name(name) {})
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product, which could be a physical product, such as a gallon of milk,
   * or a digital product, such as a book or movie, or access to an event, such
   * as a music concert or film showing.
   */
  sealed trait Product
  object Product {
    sealed trait Physical extends Product
    object Physical {
      case class Milk(gallons: Int) extends Physical
    }

    sealed trait Digital extends Product
    object Digital {
      case class Book(title: String, isbn: String) extends Digital
      case class Movie(title: String) extends Digital
    }

    sealed trait Event extends Product
    object Event {
      case class Concert(artist: String, date: Instant) extends Event
      case class Film(theater: String, movie: String, date: Instant)  extends Event
    }
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create an immutable data model
   * of a product price, which could be one-time purchase fee, or a recurring
   * fee on some regular interval.
   */
  sealed trait PricingScheme
  object PricingScheme {
    final case class OneTime(fee: BigDecimal) extends PricingScheme
    final case class Recurring(fee: BigDecimal, interval: Duration) extends PricingScheme
  }

  sealed trait ChessColor
  case object Black extends ChessColor
}

/**
 * EVENT PROCESSING - EXERCISE SET 3
 *
 * Consider an event processing application, which processes events from both
 * devices, as well as users.
 */
object events {

  /**
   * EXERCISE
   *
   * Refactor the object-oriented data model in this section to a more
   * functional one, which uses only sealed traits and case classes.
   */
  abstract class Event(val id: Int) {

    def time: Instant
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait UserEvent extends Event {
    def userName: String
  }

  // Events are either UserEvent (produced by a user) or DeviceEvent (produced by a device),
  // please don't extend both it will break code!!!
  trait DeviceEvent extends Event {
    def deviceId: Int
  }

  class SensorUpdated(id: Int, val deviceId: Int, val time: Instant, val reading: Option[Double])
      extends Event(id)
      with DeviceEvent

  class DeviceActivated(id: Int, val deviceId: Int, val time: Instant) extends Event(id) with DeviceEvent

  class UserPurchase(id: Int, val item: String, val price: Double, val time: Instant, val userName: String)
      extends Event(id)
      with UserEvent

  class UserAccountCreated(id: Int, val userName: String, val time: Instant) extends Event(id) with UserEvent

}

 /*
  * We could equally have a sum of sum of products, with a sealed trait Event{id, time}
  */
object eventsPieroWay {
  sealed trait Event
  object Event {
    case class DeviceEvent(id: Int, time: Instant, deviceId: Int, details: DeviceEvent.Details) extends Event
    object DeviceEvent {
      sealed trait Details
      object Details {
        case class SensorUpdated(reading: Option[Double]) extends Details
        case class DeviceActivated(deviceId: Int) extends Details
      }
    }
    case class UserEvent(id: Int, time: Instant, deviceId: Int, userName: String, details: UserEvent.Details) extends Event
    object UserEvent {
      sealed trait Details
      case class UserPurchase(item: String, price: Double) extends Details
      case object UserAccountCreated extends Details
    }
  }
}

// --> why not factoring out common stuff in a sealed trait? Because with this I have to repeat the shared fields
// while with a case class at top level I am able to copy stuff even without knowing the details about A
object eventsJohnsWay {
  final case class Event(id: Int, time: Instant, body: Payload)
  sealed trait Payload
  object Payload {
  sealed trait UserEvent extends Payload
    object UserEvent {
      final case class UserPurchase(val item: String, val price: Double, val userName: String) extends UserEvent
      final case class UserAccountCreated(val userName: String) extends UserEvent
    }
    
    sealed trait DeviceEvent extends Payload
    object DeviceEvent {
      final case class SensorUpdated(val deviceId: Int, val reading: Option[Double]) extends DeviceEvent
      final case class DeviceActivated(val deviceId: Int, val time: Instant) extends DeviceEvent
    }
  }
}

/**
 * DOCUMENT EDITING - EXERCISE SET 4
 *
 * Consider a web application that allows users to edit and store documents
 * of some type (which is not relevant for these exercises).
 */
object documents {
  final case class UserId(identifier: String)
  final case class DocId(identifier: String)
  final case class DocContent(body: String)

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, create a simplified but somewhat
   * realistic model of a Document.
   */
  case class Document(docId: DocId, docContent: DocContent, owner: UserId)

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, create a model of the access
   * type that a given user might have with respect to a document. For example,
   * some users might have read-only permission on a document.
   */
  sealed trait AccessType { self =>
    def includesRead: Boolean =
      self match {
        case AccessType.None => false
        case AccessType.Read => true
        case AccessType.ReadWrite => true

      }
  }
  object AccessType {
    case object None extends AccessType
    case object ReadWrite extends AccessType
    case object Read extends AccessType
  } 

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, create a model of the
   * permissions that a user has on a set of documents they have access to.
   * Do not store the document contents themselves in this model.
   */
  final case class DocPermissions(map: Map[DocId, AccessType])
}

/**
 * BANKING - EXERCISE SET 5
 *
 * Consider a banking application that allows users to hold and transfer money.
 */
object bank {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a customer at a bank.
   */
  type Customer

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of an account
   * type. For example, one account type allows the user to write checks
   * against a given currency. Another account type allows the user to earn
   * interest at a given rate for the holdings in a given currency.
   */
  type AccountType

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a bank
   * account, including details on the type of bank account, holdings, customer
   * who owns the bank account, and customers who have access to the bank account.
   */
  type Account
}

/**
 * STOCK PORTFOLIO - GRADUATION PROJECT
 *
 * Consider a web application that allows users to manage their portfolio of investments.
 */
object portfolio {

  /**
   * EXERCISE 1
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * exchange. Ensure there exist values for NASDAQ and NYSE.
   */
  sealed trait Exchange
  object Exchange {
    case object NASDAQ extends Exchange
    case object NYSE   extends Exchange
  }

  /**
   * EXERCISE 2
   *
   * Using only sealed traits and case classes, develop a model of a currency
   * type.
   */
  sealed trait CurrencyType 
  object CurrencyType {
    case object USD extends CurrencyType
    case object EUR extends CurrencyType
  }

  /**
   * EXERCISE 3
   *
   * Using only sealed traits and case classes, develop a model of a stock
   * symbol. Ensure there exists a value for Apple's stock (APPL).
   */
  final case class StockSymbol(exchange: Exchange, id: String)
    object StockSymbol {
      val APPL = StockSymbol(Exchange.NASDAQ, "APPL")
  }

  /**
   * EXERCISE 4
   *
   * Using only sealed traits and case classes, develop a model of a portfolio
   * held by a user of the web application.
   * 
   * I want to be able to combine portfolios to create another one, so Map monoid is the natural choice
   */

  final case class Holding(shares: BigDecimal, purchasePrice: BigDecimal, currencyType: CurrencyType) 
  final case class Portfolio(value: Map[StockSymbol, Set[Holding]])

  /**
   * EXERCISE 5
   *
   * Using only sealed traits and case classes, develop a model of a user of
   * the web application.
   */
  final case class User(userName: String, portfolio: Portfolio)

  /**
   * EXERCISE 6
   *
   * Using only sealed traits and case classes, develop a model of a trade type.
   * Example trade types might include Buy and Sell.
   */
  sealed trait TradeType
  object TradeType {
    case class Buy(what: What) extends TradeType
    case class Sell(what: What) extends TradeType

    sealed trait What 
    object What {
      case object Shares extends What
      case object Put extends What
    }
  }

  /**
   * EXERCISE 7
   *
   * Using only sealed traits and case classes, develop a model of a trade,
   * which involves a particular trade type of a specific stock symbol at
   * specific prices.
   */
  final case class Trade(tt: TradeType, symbol: StockSymbol, shares: BigDecimal)
}
