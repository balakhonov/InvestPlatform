package com.balakhonov.invest.analyzer

import com.balakhonov.invest.analyzer.model.AnalysisResult
import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models.StopLossSettings
import com.balakhonov.invest.models.enums.CandleTimeFrame
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.provider.ClientProvider
import com.balakhonov.invest.provider.tinkoff.TinkoffClientProvider
import com.balakhonov.invest.strategies._
import com.balakhonov.invest.util.DoubleUtil.to2p
import com.google.inject.Guice

import java.time.LocalDateTime
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.math.BigDecimal.double2bigDecimal
import scala.reflect.ClassTag
import scala.util.Random


object Main {
  private val SberBank = "BBG004730N88"
  private val Yandex = "BBG006L8G4H1"
  private val SeverStal = "BBG00475K6C3"
  private val BankVTB = "BBG004730ZJ9"
  private val BankSPB = "BBG000QJW156"
  private val Rusal = "BBG008F2T3T2"
  private val DetskiyMir = "BBG000BN56Q9"
  private val Kamaz = "BBG000LNHHJ9"
  private val Lenta = "BBG000QQPXZ5"
  private val MTC = "BBG004S681W1"
  private val PIK = "BBG004S68BH6"
  private val MSK_BIRZHA = "BBG004730JJ5"
  private val POLYMETAL = "BBG004PYF2N3"
  private val ALROSA = "BBG004S68B31"
  private val AEROFLOT = "BBG004S683W7"
  private val GAZPROM = "BBG004730RP0"
  private val LENENERGO = "BBG000NLC9Z6"
  private val M_VIDEO = "BBG004S68CP5"
  private val ROSTELECOM = "BBG004S682Z6"
  private val LUKOIL = "BBG004731032"
  private val NOVATEK = "BBG00475KKY8"
  private val ROSNEFT = "BBG004731354"
  private val RUSAGRO = "BBG007N0Z367"
  private val RUSGIDRO = "BBG00475K2X9"
  private val TATNEFT = "BBG004RVFFC0"
  private val TRUBNAYA_METALURGIYA = "BBG004TC84Z8"

  def main(args: Array[String]): Unit = {
    val pullHistoryFlag = false

    val figi = GAZPROM
    val figiSet = Seq(
      SberBank,
      Yandex,
      SeverStal,
      BankVTB,
      BankSPB,
      Rusal,
      DetskiyMir,
      Kamaz,
      Lenta,
      MTC,
      PIK,
      MSK_BIRZHA,
      POLYMETAL,
      ALROSA,
      AEROFLOT,
      GAZPROM,
      LENENERGO,
      M_VIDEO,
      ROSTELECOM,
      LUKOIL,
      NOVATEK,
      ROSNEFT,
      RUSAGRO,
      RUSGIDRO,
      TATNEFT,
      TRUBNAYA_METALURGIYA
    )

    if (pullHistoryFlag) {
      val year = 2021
      val figies = Seq(
        SberBank,
        Yandex,
        SeverStal,
        BankVTB,
        BankSPB,
        Rusal,
        DetskiyMir,
        Kamaz,
        Lenta,
        MTC,
        PIK,
        MSK_BIRZHA,
        POLYMETAL,
        ALROSA,
        AEROFLOT,
        GAZPROM,
        LENENERGO,
        M_VIDEO,
        ROSTELECOM,
        LUKOIL,
        NOVATEK,
        ROSNEFT,
        RUSAGRO,
        RUSGIDRO,
        TATNEFT,
        TRUBNAYA_METALURGIYA
      )
      val future = pullHistory[TinkoffClientProvider](figies, year, CandleTimeFrame._30MIN)

      Await.result(future, Duration.Inf)
    } else {
      val strategyAnalyzerService = getClass.getClassLoader.loadClass("com.balakhonov.invest.analyzer.StrategyAnalyzerService")

      val injector = Guice.createInjector(GuiceModule(useH2DB = false))
      val service = injector.getInstance(strategyAnalyzerService).asInstanceOf[StrategyAnalyzerService]

      val (from, to) = (
        LocalDateTime.of(2021, 1, 1, 0, 0),
        LocalDateTime.of(2021, 12, 31, 23, 59)
      )
      lazy val history = loadHistory(figi, from, to, CandleTimeFrame._10MIN)

      // val strategy = Strategy1(5, 8, 25, 40, 0.001, 0.3, 320, 0.2, StopLossSettings(enabled = true, 0.30), Left(0.1))
      val s1_1 = Strategy1(5, 8, 25, 40, 0.001, 0.3, 320, 0.2, StopLossSettings(enabled = true, 0.30), Left(0.1))

      val s2_1_1 = RSIStrategy(FirstBuySettings(1.2, 90), 0.3, 0.8, 12, 12, 120, 10, 400, 40, UptrendOptions(1.1, 17, 31), DowntrendOptions(enabled = true, 1.2, 50, 32))
      val s2_1_2 = RSIStrategy(FirstBuySettings(1.0, 85), 0.5, 0.7, 12, 12, 120, 10, 400, 40, UptrendOptions(0.4, 16, 30), DowntrendOptions(enabled = true, 0.8, 20, 29))
      val s2_1_3 = RSIStrategy(FirstBuySettings(1.0, 85), 0.5, 0.7, 12, 12, 120, 10, 400, 40, UptrendOptions(0.8, 16, 32), DowntrendOptions(enabled = true, 0.8, 20, 29))
      val s2_1_4 = RSIStrategy(FirstBuySettings(1.2, 70), 0.5, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(0.4, 18, 27), DowntrendOptions(true, 0.8, 25, 31))
      // (BestStrategy_2,7.14%,[combo] Profit: Some(7889.08) Invested: Some(110515.1) Opened: 333 Closed: 276, Non-Closed: 57
      val s2_1_5 = RSIStrategy(FirstBuySettings(1.0, 80), 0.5, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(0.6, 18, 28), DowntrendOptions(true, 1.8, 25, 29))
      // (BestStrategy_1,[combo] Profit: Some(9751.8) Invested: Some(157615.1) Opened: 446 Closed: 366, Non-Closed: 80
      val s2_1_6 = RSIStrategy(FirstBuySettings(1.2, 90), 0.7, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(0.4, 18, 29), DowntrendOptions(true, 0.8, 15, 32))
      // (BestStrategy_2,8.19%,[combo] Profit: Some(7426.02) Invested: Some(90714.75) Opened: 293 Closed: 246, Non-Closed: 47
      val s2_1_7 = RSIStrategy(FirstBuySettings(1.0, 75), 0.5, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(1.0, 14, 27), DowntrendOptions(true, 0.8, 50, 29))
      // (BestStrategy_2,8.93%,[combo] Profit: Some(17198.92) Invested: Some(192555.25) Opened: 660 Closed: 532, Non-Closed: 128
      val s2_1_8 = RSIStrategy(FirstBuySettings(1.2, 75), 0.5, 0.9, 12, 12, 120, 10, 450, 45, UptrendOptions(0.4, 18, 29), DowntrendOptions(true, 1.0, 50, 29))

      val s2_2 = RSIStrategy(FirstBuySettings(1.2, 90), 0.7, 0.8, 12, 12, 120, 10, 400, 40, UptrendOptions(0.9, 17, 32), DowntrendOptions(enabled = false, 0.6, 45, 27))
      val s2_3 = RSIStrategy(FirstBuySettings(1.2, 75), 0.5, 0.9, 12, 12, 120, 10, 400, 40, UptrendOptions(1.0, 16, 31), DowntrendOptions(enabled = false, 0.8, 15, 30))
      val s2_4 = RSIStrategy(FirstBuySettings(1.2, 75), 0.7, 0.9, 12, 12, 120, 10, 400, 40, UptrendOptions(1.0, 16, 32), DowntrendOptions(false, 0.8, 15, 30))
      val s2_5 = RSIStrategy(FirstBuySettings(1.2, 80), 0.5, 0.9, 12, 12, 120, 10, 400, 40, UptrendOptions(0.6, 20, 28), DowntrendOptions(false, 0.8, 15, 30))

      def run(strategy: Strategy) = {
        service.analyse(
          figi = figi,
          strategy = strategy,
          history = history
        )
      }

//      val result1 = run(s1_1)
//      val result2 = run(s2_1_1)
//      val result3 = run(s2_1_2)
//      val result4 = run(s2_1_3)
//      val result5 = run(s2_1_4)
//      val result6 = run(s2_1_5)
//      val result7 = run(s2_1_6)
//      val result8 = run(s2_1_7)
//      val result9 = run(s2_1_8)
//      val result10 = run(s2_2)
//      val result11 = run(s2_3)
//      val result12 = run(s2_4)
//      val result13 = run(s2_5)
//      println(result1)
//      println(result2)
//      println(result3)
//      println(result4)
//      println(result5)
//      println(result6)
//      println(result7)
//      println(result8)
//      println(result9)
//      println("--------")
//      println(result10)
//      println(result11)
//      println(result12)
//      println(result13)
//      println("----END----")

      val MinThresholdToBuy = 0.4

      val fbSettings = Random.shuffle((for (percDeficitAgainstHighestPrice <- 1.0 to 1.2 by 0.2) yield
        for (numberOfBarsToCheck <- 70 to 90 by 5) yield
          FirstBuySettings(
            percDeficitAgainstHighestPrice = percDeficitAgainstHighestPrice.doubleValue,
            numberOfBarsToCheck = numberOfBarsToCheck
          )
        ).toList.flatten)

      val utOptions = Random.shuffle((for (thresholdToBuy <- MinThresholdToBuy to 1.2 by 0.2) yield
        for (rsiPeriod <- 14 to 20 by 2) yield
          for (rsiLowThreshold <- 27 to 32 by 1) yield {
            UptrendOptions(
              thresholdToBuy = thresholdToBuy.doubleValue,
              rsiPeriod = rsiPeriod,
              rsiLowThreshold = rsiLowThreshold
            )
          }).toList.flatten.flatten)

      val dtOptions = Random.shuffle((
        for (thresholdToBuy <- 0.8 to 2.0 by 0.2) yield
          for (rsiPeriod <- 15 to 50 by 5) yield
            for (rsiLowThreshold <- 29 to 32 by 1) yield {
              DowntrendOptions(
                enabled = true,
                thresholdToBuy = thresholdToBuy.doubleValue,
                rsiPeriod = rsiPeriod,
                rsiLowThreshold = rsiLowThreshold
              )
            }
        ).toList.flatten.flatten)

      //      val dtOptions = List(DowntrendOptions(
      //        enabled = false,
      //        thresholdToBuy = 0.8,
      //        rsiPeriod = 15,
      //        rsiLowThreshold = 30
      //      ))

      case class DeltaToSellSettings(minDeltaPercToSell: Double,
                                     adjustedDeltaPercToSellMultiplier: Double)

      val dtsSettings = Random.shuffle((for (minDeltaPercToSell <- 0.5 to 0.7 by 0.2) yield
        for (adjustedDeltaPercToSellMultiplier <- 0.7 to 0.9 by 0.1) yield
          DeltaToSellSettings(
            minDeltaPercToSell = minDeltaPercToSell.doubleValue,
            adjustedDeltaPercToSellMultiplier = adjustedDeltaPercToSellMultiplier.doubleValue
          )
        ).toList.flatten)

      var index = 1
      val totalStrategies = utOptions.size * dtOptions.size * fbSettings.size * dtsSettings.size
      var bestStrategy1 = AnalysisResult(
        figi = figi,
        profit = None,
        invested = None,
        opened = 0,
        closed = 0,
        nonClosed = 0,
        strategy = None.orNull,
        children = Nil
      )
      var bestStrategy2 = AnalysisResult(
        figi = figi,
        profit = None,
        invested = None,
        opened = 0,
        closed = 0,
        nonClosed = 0,
        strategy = None.orNull,
        children = Nil
      )

      val analyzer = service.analyse(
        figiSet = figiSet,
        from = from,
        to = to
      )

      var strategies = {
        val buffer = ArrayBuffer.newBuilder[(UptrendOptions, DowntrendOptions, FirstBuySettings, DeltaToSellSettings)]
        buffer.sizeHint(70000000)
        for (uptrendOptions <- utOptions)
          for (downtrendOptions <- dtOptions)
            for (firstBuySettings <- fbSettings)
              for (_dtsSettings <- dtsSettings) {
                buffer.+=((uptrendOptions, downtrendOptions, firstBuySettings, _dtsSettings))
              }
        buffer.result()
      }

      strategies = Random.shuffle(strategies)

      strategies.foreach { case (uptrendOptions, downtrendOptions, firstBuySettings, dtsSettings) =>
        val strategy = RSIStrategy(
          firstBuySettings = firstBuySettings,
          minDeltaPercToSell = dtsSettings.minDeltaPercToSell,
          adjustedDeltaPercToSellMultiplier = dtsSettings.adjustedDeltaPercToSellMultiplier,
          prevP1TrendBars = 12,
          currentP1TrendBars = 12,
          currentP2TrendBars = 120,
          currentP2TrendMultiplier = 10,
          currentP3TrendBars = 450,
          currentP3TrendMultiplier = 45,
          uptrendOptions = uptrendOptions,
          downtrendOptions = downtrendOptions
        )
        val res = analyzer(s2_1_8)

        if (res.profit.exists(p => bestStrategy1.profit.forall(_ < p))) {
          println("BestStrategy_1", res)
          bestStrategy1 = res
        } else if (res.profit.exists(p => bestStrategy1.profit.forall(_ == p)) && res.nonClosed < bestStrategy1.nonClosed) {
          println("BestStrategy_1", res)
          bestStrategy1 = res
        } else {
          //
        }

        def calcPerc(res: AnalysisResult): Double = {
          res.invested match {
            case Some(invested) =>
              val profit = res.profit.getOrElse(0d)
              profit / invested

            case None => res.profit.fold(0)(_ => 1)
          }
        }

        if (calcPerc(bestStrategy2) < calcPerc(res)) {
          println("BestStrategy_2", s"${to2p(calcPerc(res) * 100)}%", res)
          bestStrategy2 = res
        } else {
          //
        }

        if (index % 20 == 0)
          println(s"Processed $index from $totalStrategies")

        index += 1
      }
    }
  }

  private def pullHistory[T](figies: Seq[String],
                             year: Int,
                             timeFrame: CandleTimeFrame)
                            (implicit ict: ClassTag[T]): Future[Unit] = {
    val injector = Guice.createInjector(GuiceModule(useH2DB = false))
    val serviceClass = getClass.getClassLoader.loadClass("com.balakhonov.invest.analyzer.DownloadHistoryService")
    val providerClass = getClass.getClassLoader.loadClass(ict.runtimeClass.getName)
    val service = injector.getInstance(serviceClass).asInstanceOf[DownloadHistoryService]
    val provider = injector.getInstance(providerClass).asInstanceOf[ClientProvider]

    service.downloadByYear(figies, year, timeFrame)(provider)
  }

  private def loadHistory(figi: String,
                          from: LocalDateTime,
                          to: LocalDateTime,
                          interval: CandleTimeFrame) = {
    val injector = Guice.createInjector(GuiceModule(useH2DB = false))
    val clazz = getClass.getClassLoader.loadClass("com.balakhonov.invest.dao.UtilDAO")
    val utilDAO = injector.getInstance(clazz).asInstanceOf[UtilDAO]

    utilDAO.candleDao.list(figi, from, to, interval)
  }
}