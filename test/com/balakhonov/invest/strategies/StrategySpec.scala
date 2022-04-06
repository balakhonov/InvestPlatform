package com.balakhonov.invest.strategies

import cats.implicits.catsSyntaxOptionId
import com.balakhonov.invest.models.TradeContext
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.services.OrderService
import org.mockito.ArgumentMatchers.{eq => eqs}
import org.specs2.mock.Mockito
import org.specs2.mutable.SpecificationLike

import java.time.{LocalDate, LocalDateTime, Month}

class StrategySpec
  extends SpecificationLike
    with Mockito {

  "StrategySpec" should {

    "#test 1MIN" in {
      implicit val ctx: TradeContext = mock[TradeContext]
      val orderService = mock[OrderService]

      val date1 = LocalDate.of(2022, Month.JANUARY, 1)
      val date2 = LocalDate.of(2022, Month.JANUARY, 2)

      val candle1 = createCandle(date1.atTime(7, 1, 1), 100)
      val candle2 = createCandle(date1.atTime(7, 2, 30), 101)
      val candle3 = createCandle(date1.atTime(7, 2, 1), 102) // update
      val candle4 = createCandle(date1.atTime(7, 3, 1), 103)
      val candle5 = createCandle(date1.atTime(7, 3, 59), 103)
      val candle6 = createCandle(date1.atTime(7, 5, 1), 105)
      val candle7 = createCandle(date1.atTime(7, 5, 59), 106) // update
      val candle8 = createCandle(date1.atTime(7, 7, 1), 107)
      val candle9 = createCandle(date1.atTime(8, 1, 1), 108)
      val candle10 = createCandle(date1.atTime(8, 1, 59), 109)
      val candle11 = createCandle(date1.atTime(8, 2, 1), 110)
      val candle12 = createCandle(date2.atTime(7, 1, 1), 111)
      val candle13 = createCandle(date2.atTime(7, 1, 59), 112) // update
      val candle14 = createCandle(date2.atTime(7, 2, 1), 113)

      val strategy = spy(DummyStrategy(CandleTimeFrame._1MIN))
      strategy.setOrderService(orderService)
      strategy.orderService must_== orderService

      strategy.setCandlesQueue(Seq(candle3, candle4))
      strategy.candlesQueue must have size 2
      strategy.historySize must_== 2
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle4)

      strategy.setCandlesQueue(Seq(candle2, candle1))
      strategy.candlesQueue must have size 2
      strategy.historySize must_== 2
      strategy.lastPrice must_== 101.some
      strategy.lastCandle must beSome(candle2)
      there was no(strategy).beforeProcess(any)(any)
      there was no(strategy).afterProcess(any)(any)
      there was no(strategy).receive(any)(any)

      strategy.doReceive(candle3)
      strategy.historySize must_== 2
      strategy.lastPrice must_== 102.some
      strategy.lastCandle must beSome(candle3)
      there was one(strategy).beforeProcess(eqs(candle3))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle3))(eqs(ctx))
      there was one(strategy).receive(eqs(candle3))(eqs(ctx))

      strategy.doReceive(candle4)
      strategy.historySize must_== 3
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle4)
      there was one(strategy).beforeProcess(eqs(candle4))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle4))(eqs(ctx))
      there was one(strategy).receive(eqs(candle4))(eqs(ctx))

      strategy.doReceive(candle5)
      strategy.historySize must_== 3
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle5)
      there was no(strategy).beforeProcess(eqs(candle5))(eqs(ctx))
      there was no(strategy).afterProcess(eqs(candle5))(eqs(ctx))
      there was no(strategy).receive(eqs(candle5))(eqs(ctx))

      strategy.doReceive(candle6)
      strategy.historySize must_== 4
      strategy.lastPrice must_== 105.some
      strategy.lastCandle must beSome(candle6)
      there was one(strategy).beforeProcess(eqs(candle6))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle6))(eqs(ctx))
      there was one(strategy).receive(eqs(candle6))(eqs(ctx))

      strategy.doReceive(candle7)
      strategy.doReceive(candle8)
      strategy.doReceive(candle9)
      strategy.doReceive(candle10)
      strategy.doReceive(candle11)
      strategy.doReceive(candle12)
      strategy.doReceive(candle13)
      strategy.doReceive(candle14)

      strategy.candlesQueue must have size 5
      strategy.historySize must_== 5
      strategy.lastPrice must_== 113.some
    }

    "#test 10MIN" in {
      implicit val ctx: TradeContext = mock[TradeContext]
      val orderService = mock[OrderService]

      val date1 = LocalDate.of(2022, Month.JANUARY, 1)
      val date2 = LocalDate.of(2022, Month.JANUARY, 2)

      val candle1 = createCandle(date1.atTime(7, 0, 1), 100)
      val candle2 = createCandle(date1.atTime(7, 10, 30), 101)
      val candle3 = createCandle(date1.atTime(7, 19, 1), 102) // update
      val candle4 = createCandle(date1.atTime(7, 20, 1), 103)
      val candle5 = createCandle(date1.atTime(7, 29, 59), 103)
      val candle6 = createCandle(date1.atTime(7, 30, 1), 105)
      val candle7 = createCandle(date1.atTime(7, 39, 59), 106) // update
      val candle8 = createCandle(date1.atTime(7, 50, 1), 107)
      val candle9 = createCandle(date1.atTime(8, 0, 1), 108) // update
      val candle10 = createCandle(date1.atTime(8, 0, 1), 109)
      val candle11 = createCandle(date1.atTime(8, 10, 1), 110)
      val candle12 = createCandle(date2.atTime(7, 0, 1), 111)
      val candle13 = createCandle(date2.atTime(7, 9, 59), 112) // update
      val candle14 = createCandle(date2.atTime(7, 10, 1), 113)

      val strategy = spy(DummyStrategy(CandleTimeFrame._10MIN))
      strategy.setOrderService(orderService)
      strategy.orderService must_== orderService

      strategy.setCandlesQueue(Seq(candle3, candle4))
      strategy.candlesQueue must have size 2
      strategy.historySize must_== 2
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle4)

      strategy.setCandlesQueue(Seq(candle2, candle1))
      strategy.candlesQueue must have size 2
      strategy.historySize must_== 2
      strategy.lastPrice must_== 101.some
      strategy.lastCandle must beSome(candle2)
      there was no(strategy).beforeProcess(any)(any)
      there was no(strategy).afterProcess(any)(any)
      there was no(strategy).receive(any)(any)

      strategy.doReceive(candle3)
      strategy.historySize must_== 2
      strategy.lastPrice must_== 102.some
      strategy.lastCandle must beSome(candle3)
      there was one(strategy).beforeProcess(eqs(candle3))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle3))(eqs(ctx))
      there was one(strategy).receive(eqs(candle3))(eqs(ctx))

      strategy.doReceive(candle4)
      strategy.historySize must_== 3
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle4)
      there was one(strategy).beforeProcess(eqs(candle4))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle4))(eqs(ctx))
      there was one(strategy).receive(eqs(candle4))(eqs(ctx))

      strategy.doReceive(candle5)
      strategy.historySize must_== 3
      strategy.lastPrice must_== 103.some
      strategy.lastCandle must beSome(candle5)
      there was no(strategy).beforeProcess(eqs(candle5))(eqs(ctx))
      there was no(strategy).afterProcess(eqs(candle5))(eqs(ctx))
      there was no(strategy).receive(eqs(candle5))(eqs(ctx))

      strategy.doReceive(candle6)
      strategy.historySize must_== 4
      strategy.lastPrice must_== 105.some
      strategy.lastCandle must beSome(candle6)
      there was one(strategy).beforeProcess(eqs(candle6))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle6))(eqs(ctx))
      there was one(strategy).receive(eqs(candle6))(eqs(ctx))

      strategy.doReceive(candle7)
      strategy.doReceive(candle8)

      strategy.doReceive(candle9)
      strategy.historySize must_== 5
      strategy.lastPrice must_== 108.some
      strategy.lastCandle must beSome(candle9)
      there was one(strategy).beforeProcess(eqs(candle9))(eqs(ctx))
      there was one(strategy).afterProcess(eqs(candle9))(eqs(ctx))
      there was one(strategy).receive(eqs(candle9))(eqs(ctx))

      strategy.doReceive(candle10)
      strategy.doReceive(candle11)
      strategy.doReceive(candle12)
      strategy.doReceive(candle13)
      strategy.doReceive(candle14)

      strategy.candlesQueue must have size 5
      strategy.historySize must_== 5
      strategy.lastPrice must_== 113.some
    }
  }

  private def createCandle(dateTime: LocalDateTime,
                           closingPrice: Double) = {
    Candle(
      id = 0,
      openPrice = 100,
      closingPrice = closingPrice,
      highestPrice = 100,
      lowestPrice = 100,
      tradingValue = 0,
      dateTime = dateTime,
      interval = CandleTimeFrame._1MIN,
      figi = "FIGI"
    )
  }

  case class DummyStrategy(override val TimeFrame: CandleTimeFrame)
    extends Strategy {
    override val MaxCandlesQueueSize: Int = 5

    override def beforeProcess(candle: Candle)(implicit ctx: TradeContext): Unit = {

    }

    override def afterProcess(candle: Candle)(implicit ctx: TradeContext): Unit = {

    }

    override def receive(candle: Candle)(implicit ctx: TradeContext): Unit = {

    }
  }
}

