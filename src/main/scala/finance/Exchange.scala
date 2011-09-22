package finance

import java.util.Date
import math._
import collection.immutable.SortedMap

case class OrderCode(code : String)
case class TradeCode(code : String)
object TradeCode {
  private var tradeId = 0
  def unassigned  = TradeCode("unassigned")
  def nextId = { tradeId = tradeId + 1; TradeCode("tc-" + tradeId) }
}
trait Instrument{
  val name : String
}
case class DefaultInstrument(name : String) extends Instrument
object Instrument {
  case object BARC extends Instrument { val name = "BARC" }
}
trait OrderRequest {
  val instrument : Instrument
  val direction : Direction
  val orderType : OrderType
  val quantity : Quantity
  val price : Price
  val counterparty : Counterparty
}
case class DefaultOrderRequest(instrument : Instrument,
                               direction : Direction,
                               orderType : OrderType,
                               quantity : Quantity,
                               price : Price,
                               counterparty : Counterparty) extends OrderRequest
object OrderRequest {
  def apply(instrument : Instrument, direction : Direction, quantity : Quantity, price : Price, orderType : OrderType = LimitOrder) : OrderRequest =
    DefaultOrderRequest(instrument, direction, orderType, quantity, price, Counterparty.Null)
}
case class Order(orderCode : OrderCode,
                 instrument : Instrument,
                 direction : Direction,
                 orderType : OrderType = LimitOrder,
                 originalQuantity : Quantity,
                 quantity : Quantity,
                 price : Price,
                 time : Date,
                 counterparty : Counterparty,
                 status : OrderStatus = OrderOpen) {
  override def toString : String = {
    "%s (%s) %f @ %f %s %s Unallocated Qty = %f %s".format(instrument.name, direction.toString(), originalQuantity.value, price.value, orderType, status, quantity.value, time.toString)
  }
  def asBookString : String = {
    if (direction == Purchase)
      "%f \t%f".format(quantity.value, price.value)
    else
      "%f \t%f".format(price.value, quantity.value)
  }
}
trait OrderType { val name : String }
case object MarketOrder extends OrderType { val name = "MO" }
case object LimitOrder extends OrderType { val name = "L" }
case object FillOrKillOrder extends OrderType { val name = "FoK" }
trait OrderStatus
case object OrderOpen extends OrderStatus
case object OrderClosed extends OrderStatus
object Order {
  def apply(orderCode : OrderCode, orderRequest : OrderRequest) : Order =
    Order(orderCode,
          orderRequest.instrument,
          orderRequest.direction,
          orderRequest.orderType,
          orderRequest.quantity,
          orderRequest.quantity,
          orderRequest.price,
          new Date(),
          Counterparty.Null)
}
case class Trade(tradeCode : TradeCode,
                 quantity : Quantity, price : Price,
                 order : Order,
                 timestamp : Date,
                 buyCounterparty : Counterparty,
                 sellCounterparty : Counterparty)
trait Counterparty {
  val name : String
  val account : Account
}
case class DefaultCounterparty(name : String, account : Account = Account.Null)
object Counterparty {
  case object Null extends Counterparty { val name = "Null-Counterparty"; val account = Account.Null }
}
trait Account
object Account {
  case object Null extends Account
}
trait Direction
case object Sale extends Direction {
  override def toString = "S"
}
case object Purchase extends Direction {
  override def toString = "P"
}
trait TradeLogViewer {
  def getTrades() : List[Trade]
}
trait TradeLog {
  def addTrade(trade : Trade)
  def registerTradeListener(listener : List[TradeEvent] => Unit)
}
case class DefaultTradeLog() extends TradeLog with TradeLogViewer {
  implicit object DefaultOrder extends Ordering[Trade] {
    def compare(k1: Trade, k2: Trade) =
      k2.timestamp.compareTo(k1.timestamp)
  }

  private var trades : Map[TradeCode, Trade] = Map[TradeCode, Trade]()
  def addTrade(trade : Trade) = {
    val updatedTrade = trade.copy(tradeCode = TradeCode.nextId)
    trades += updatedTrade.tradeCode -> updatedTrade
    val tradeEvents : List[TradeEvent] = List(TradeEvent(updatedTrade))
    tradeListeners.foreach(tl => tl(tradeEvents))
  }
  def getTrades() = {
    trades.values.toList.sortWith((t1, t2) => t1.timestamp.after(t2.timestamp))
  }
  private var tradeListeners = List[List[TradeEvent] => Unit]()
  def registerTradeListener(listener : List[TradeEvent] => Unit) {
    tradeListeners = listener :: tradeListeners
  }
}

case class PriceUpdatedEvent(direction : Direction, price : Price, quantity : Quantity)
case class TradeEvent(trade : Trade)

trait TouchItem {
  val direction : Direction
  val quantity : Quantity
  val price : Price
  val depth : Int
  def asTouchString : String
}
case class DefaultTouchItem(direction : Direction, quantity : Quantity, price : Price, depth : Int) extends TouchItem {
  def asTouchString : String = {
    if (direction == Purchase)
      "%d \t%f \t%f".format(depth, quantity.value, price.value)
    else
      "%f \t%f \t%d".format(price.value, quantity.value, depth)
  }
}
case object TouchItem {
  case object Null extends TouchItem {
    val direction = Purchase
    val quantity = Quantity()
    val price = Price()
    val depth = 0
    def asTouchString = "-"
  }
}
trait Touch {
  def bidTouch : TouchItem
  def offerTouch : TouchItem
}
case class DefaultTouch(bidTouch : TouchItem, offerTouch : TouchItem) extends Touch {
  override def toString = {
    "%s \t|\t %s".format(bidTouch.asTouchString, offerTouch.asTouchString)
  }
}

trait OrderBook {
  val books : Map[Direction, SingleBook]
  def depth(direction : Direction) : Int
  def totalVol(direction : Direction) : Quantity
  def addOrder(order : Order) : Order
  //def updateOrder(order : Order)
  //def deleteOrder(order : Order)
  def touch : Touch
  def registerPriceUpdatedListener(listener : List[PriceUpdatedEvent] => Unit)
}

trait SingleBook {
  val orders : collection.mutable.Map[OrderCode, Order]
  var sortedOrders : SortedMap[Price, List[Order]]
  val direction : Direction
  def depth : Int
  def totalVol : Quantity
  def vwap : Price
  def addOrder(order : Order)
  def updateOrder(order : Order)
  def deleteOrder(order : Order)
  def directionStr = if (direction == Purchase) "Bid" else "Offer"
}

case class DefaultSingleBook(direction : Direction) extends SingleBook {
  //val keyOrdering = implicitly[Ordering[Price]]
  implicit object DefaultOrder extends Ordering[Price] {
    def compare(k1: Price, k2: Price) =
      if (direction == Purchase)
        k2.value.compare(k1.value)
      else
        k1.value.compare(k2.value) // keyOrdering.compare(k1, k2)
  }
  val orders : collection.mutable.Map[OrderCode, Order] = collection.mutable.Map[OrderCode, Order]()
  var sortedOrders = SortedMap[Price, List[Order]]()

  def depth = sortedOrders.values.map(_.size).sum
  def totalVol : Quantity = Quantity(sortedOrders.values.flatMap(_.map(_.quantity.value)).sum)
  def vwap = Price(0.0, Currency.Null) // todo...

  def addOrder(order : Order) = {
    val priceOrders : List[Order] = sortedOrders.getOrElse(order.price, List())
    sortedOrders += order.price -> (order :: priceOrders)
    orders += order.orderCode -> order
  }
  def updateOrder(order : Order) = {
    deleteOrder(order)
    addOrder(order)
  }
  def deleteOrder(order : Order) = {
    sortedOrders.getOrElse(order.price, Nil).filter(o => o.orderCode != order.orderCode) match {
      case Nil => sortedOrders -= order.price
      case updatedOrders => sortedOrders += order.price -> updatedOrders
    }
    orders -= order.orderCode
  }

  override def toString : String =
    "%s order book \n%s".format(directionStr, sortedOrders.map(p => p._2.map(_.asBookString).mkString("\n")).mkString("\n"))
}

case class DefaultOrderBook(instrument : Instrument, exchange : Exchange, tradeLog : TradeLog) extends OrderBook {
  val bidBook = DefaultSingleBook(Purchase)
  val offerBook = DefaultSingleBook(Sale)
  val books : Map[Direction, SingleBook] = Map(Purchase -> bidBook, Sale -> offerBook)
  val matcher = DefaultMatchingEngine(bidBook, offerBook, tradeLog)
  def depth(direction : Direction) : Int = books(direction).depth
  def totalVol(direction : Direction) : Quantity = books(direction).totalVol

  def addOrder(order : Order) = {
    val currentTouch = touch
    val book = books(order.direction)
    val updatedOrder = matcher.matching(order)
    if (updatedOrder.quantity.value > 0 && updatedOrder.status != OrderClosed) {
      book.addOrder(updatedOrder)
      val updatedTouch = touch
      updatePriceEvents(currentTouch, updatedTouch)
    }
    updatedOrder
  }

  private def updatePriceEvents(currentTouch : Touch, updatedTouch : Touch) = {
    val bidEvent = if (updatedTouch.bidTouch.price != currentTouch.bidTouch.price)
        Some(PriceUpdatedEvent(Purchase, updatedTouch.bidTouch.price.copy(value = updatedTouch.bidTouch.price.value - currentTouch.bidTouch.price.value), updatedTouch.bidTouch.quantity))
      else None
    val offerEvent = if (updatedTouch.offerTouch.price != currentTouch.offerTouch.price)
        Some(PriceUpdatedEvent(Sale, updatedTouch.offerTouch.price.copy(value = updatedTouch.offerTouch.price.value - currentTouch.offerTouch.price.value), updatedTouch.offerTouch.quantity))
      else None

    val events = bidEvent.toList ::: offerEvent.toList
    if (events.size > 0)
      priceListeners.foreach(pl => pl(events))
  }

  def touch : Touch = {
    def getTouch(book : SingleBook) : TouchItem = {
      val touchItem = book.sortedOrders.headOption
        .map(p => (p._1, Quantity(p._2.map(_.quantity.value).sum), p._2.size))
        .map(p => DefaultTouchItem(book.direction, p._2, p._1, p._3))
      touchItem.getOrElse(TouchItem.Null)
    }
    DefaultTouch(getTouch(bidBook), getTouch(offerBook))
  }

  private var priceListeners = List[List[PriceUpdatedEvent] => Unit]()
  def registerPriceUpdatedListener(listener : List[PriceUpdatedEvent] => Unit) {
    priceListeners = listener :: priceListeners
  }

  override def toString : String =
    "Order book:\n%s\n%s".format(bidBook, offerBook)
}

class OrderTypeNotSupportedException(order : Order) extends Exception
trait MatchingEngine {
  val bidBook : SingleBook
  val offerBook : SingleBook
  val tradeLog : TradeLog
  def matching(order : Order) : Order
}

case class DefaultMatchingEngine(
      bidBook : SingleBook,
      offerBook : SingleBook,
      tradeLog : TradeLog) extends MatchingEngine {
  def matching(order : Order) = {
    val (book, matchedPriceOrders) = order.direction match {
      case Purchase => {
        (offerBook, offerBook.sortedOrders.filter(_._1.value <= order.price.value))
      }
      case Sale => {
        (bidBook, bidBook.sortedOrders.filter(_._1.value >= order.price.value))
      }
    }
    order.orderType match {
      case LimitOrder => {
        limitOrder(book, order, matchedPriceOrders)
          /*
          offerBook.sortedOrders.filter(kv => kv._1.value <= order.price.value).values.flatten match {
            case Nil => order
            case matchedOrders => {
              //println(matchedOrders)
              var remainingQuantity = order.quantity.value
              matchedOrders.toList.foreach(matchedOrder => {
                if (remainingQuantity > 0) {
                  val matchedQuantity = if ((remainingQuantity - matchedOrder.quantity.value) <= 0)
                    remainingQuantity
                  else
                    matchedOrder.quantity.value
                  val updatedMatchedOrder = matchedOrder.copy(quantity = Quantity(matchedOrder.quantity.value - matchedQuantity, matchedOrder.quantity.units))
                  if (updatedMatchedOrder.quantity.value > 0)
                    offerBook.updateOrder(updatedMatchedOrder)
                  else
                    offerBook.deleteOrder(updatedMatchedOrder)

                  val tcode = TradeCode("dummy-tradecode") // todo
                  val trade = Trade(tcode, order, Quantity(matchedQuantity, order.quantity.units), order.price, order.counterparty, matchedOrder.counterparty)
                  tradeLog.addTrade(trade)
                  remainingQuantity -= matchedQuantity
                }
              })
              order.copy(quantity = order.quantity.copy(value = remainingQuantity))
            }
          }
          */
          /*
          offerBook.sortedOrders.keys.filter(_.value <= order.price.value) match {
            case Nil => order
            case matchedOrders => {
              //println(matchedOrders)
              var remainingQuantity = order.quantity.value
              matchedOrders.toList.foreach(orderPriceMatch => {
                offerBook.sortedOrders(orderPriceMatch).toList.foreach(matchedOrder => {
                  if (remainingQuantity > 0) {
                    val matchedQuantity = if ((remainingQuantity - matchedOrder.quantity.value) <= 0)
                      remainingQuantity
                    else
                      matchedOrder.quantity.value
                    val updatedMatchedOrder = matchedOrder.copy(quantity = Quantity(matchedOrder.quantity.value - matchedQuantity, matchedOrder.quantity.units))
                    if (updatedMatchedOrder.quantity.value > 0)
                      offerBook.updateOrder(updatedMatchedOrder)
                    else
                      offerBook.deleteOrder(updatedMatchedOrder)

                    val tcode = TradeCode("dummy-tradecode") // todo
                    val trade = Trade(tcode, order, Quantity(matchedQuantity, order.quantity.units), order.price, order.counterparty, matchedOrder.counterparty)
                    tradeLog.addTrade(trade)
                    remainingQuantity -= matchedQuantity
                  }
                })
              })
              order.copy(quantity = order.quantity.copy(value = remainingQuantity))
            }
          }  */
        }
        case MarketOrder => {
          if (book.sortedOrders.size <= 0) throw new Exception("Market order with no defined price currently unsupported")
          val priceQuantities = book.sortedOrders.map(p => p._1.value -> p._2.map(_.quantity.value).sum)

          def getPrice(qty : Double, bestPrice : Double, priceQtys : Map[Double, Double]) : Double = {
            if (priceQtys.size <= 0 || qty <= priceQtys.head._2) bestPrice
            else {
              getPrice(qty - priceQtys.values.head, priceQtys.keys.head, priceQtys.tail)
            }
          }
          val  pricePoint = getPrice(order.quantity.value, 0, priceQuantities)
          limitOrder(book, order.copy(price = order.price.copy(value = pricePoint)), book.sortedOrders)
        }
        case FillOrKillOrder => {
          val availableQuantity = matchedPriceOrders.values.flatten.map(_.quantity.value).sum
          if (availableQuantity >= order.quantity.value){
            limitOrder(book, order, matchedPriceOrders)
          }
          else
            order.copy(status = OrderClosed)
        }
        case _ => throw new OrderTypeNotSupportedException(order)
    }
  }

  def limitOrder(book : SingleBook, order : Order, matchingOrders : Map[Price, List[Order]]) = {
    matchingOrders.values.flatten match {
      case Nil => order
      case matchedOrders => {
        //println(matchedOrders)
        var remainingQuantity = order.quantity.value
        matchedOrders.toList.foreach(matchedOrder => {
          if (remainingQuantity > 0) {
            val matchedQuantity = if ((remainingQuantity - matchedOrder.quantity.value) <= 0)
              remainingQuantity
            else
              matchedOrder.quantity.value
            val updatedMatchedOrder = matchedOrder.copy(quantity = Quantity(matchedOrder.quantity.value - matchedQuantity, matchedOrder.quantity.units))
            if (updatedMatchedOrder.quantity.value > 0)
              book.updateOrder(updatedMatchedOrder)
            else
              book.deleteOrder(updatedMatchedOrder)

            val trade = Trade(TradeCode.unassigned, Quantity(matchedQuantity, order.quantity.units), order.price, order, new Date(), order.counterparty, matchedOrder.counterparty)
            tradeLog.addTrade(trade)
            remainingQuantity -= matchedQuantity
          }
        })
        order.copy(quantity = order.quantity.copy(value = remainingQuantity), status = if (remainingQuantity > 0) OrderOpen else OrderClosed)
      }
    }
  }
}

trait Exchange {
  val id : String
  //val tradeLog : TradeLog
  def getTradeLogViewer() : TradeLogViewer
  def orderBook(instrument : Instrument) : OrderBook
  def addOrder(orderRequest : OrderRequest) : Order
}

case class DefaultExchange(id : String, name : String, currency : Currency) extends Exchange {
  private val orderBooks = collection.mutable.Map[Instrument, OrderBook]()
  private val tradeLog = new DefaultTradeLog()

  def orderBook(instrument : Instrument) : OrderBook = orderBooks(instrument)

  def addOrder(orderRequest : OrderRequest) : Order = {
    val book = orderBooks.getOrElseUpdate(orderRequest.instrument, DefaultOrderBook(orderRequest.instrument, this, this.tradeLog))

    val code = OrderCode(id + "-" + System.currentTimeMillis() + "-" + Exchange.nextSeqId)
    val order = Order(code, orderRequest)
    val updatedOrder = book.addOrder(order)
    updatedOrder
  }

  def registerTradeListener(listener : List[TradeEvent] => Unit) {
    tradeLog.registerTradeListener(listener)
  }

  def getTradeLogViewer() : TradeLogViewer = new TradeLogViewer {
    def getTrades = tradeLog.getTrades()
  }
}

object Exchange {
  private var seqId = 0
  val nextSeqId = { seqId = seqId + 1; seqId }
}

object TestExchange {
  def main(args : Array[String]) {
    val exchange = DefaultExchange("LEX", "Louis Exchange1", Currency.GBP)

    def placeOrder(exchange : Exchange)(instrument : Instrument)(orderRequest : OrderRequest) = {
      val order = exchange.addOrder(orderRequest)
      println("Order " + order)
    }
    val addOrder = placeOrder(exchange)(Instrument.BARC) _

    def status(book : OrderBook) = {
      println("\nCurrent book depth %d, bid vol %f | %f offer vol, %d depth".format(book.depth(Purchase), book.totalVol(Purchase).value, book.totalVol(Sale).value, book.depth(Sale)))
      println(book.toString())

      println("\nTouch")
      println(book.touch.toString)
    }

    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(100.0), Price(25.0, Currency.GBP)))
    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(99.0), Price(15.0, Currency.GBP)))

    val book = exchange.orderBook(Instrument.BARC)

    book.registerPriceUpdatedListener((p : List[PriceUpdatedEvent]) => {
      val msg = p.mkString(", ")
      println("*****> Price changed events: " + msg)
    })

    exchange.registerTradeListener((t : List[TradeEvent]) => {
      val msg = t.mkString(", ")
      println("*****> Trade events: " + msg)
    })


    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(1000.0), Price(10.0, Currency.GBP)))

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(250.0), Price(10.0, Currency.GBP)))

    println("\nresults...")

    val bidBook = book.books(Purchase)
    val offerBook = book.books(Purchase)

    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(1000.0), Price(11.0, Currency.GBP)))

    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(250.0), Price(12.0, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(100.0), Price(15.0, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(200.0), Price(16.0, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(1000.0), Price(14.0, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(2000.0), Price(14.5, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Sale, Quantity(250.0), Price(14.0, Currency.GBP)))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(2500.0), Price(15.0, Currency.GBP), FillOrKillOrder))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(2000.0), Price(14.0, Currency.GBP), FillOrKillOrder))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(2200.0), Price(0.0, Currency.GBP), MarketOrder))
    status(book)

    addOrder(OrderRequest(Instrument.BARC, Purchase, Quantity(500.0), Price(0.0, Currency.GBP), MarketOrder))
    status(book)

    println("All trades:")
    val trades = exchange.getTradeLogViewer().getTrades()
    trades.foreach(println)
  }
}
