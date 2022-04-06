package com.balakhonov.invest.util

import com.balakhonov.invest.models.TradeContext
import com.balakhonov.invest.models.db.Candle
import com.balakhonov.invest.util.ColorHelper.{withGREEN, withRED, withWHITE}
import com.balakhonov.invest.util.DoubleUtil.to2p
import com.balakhonov.invest.{Downtrend, Flatline, Trend, Uptrend}

object ConsoleUtil {

  def trends(trends: Seq[Trend]): String = {
    def colored(trend: Trend): String = {
      val s = "%-15s".format(trend.toString)
      trend match {
        case Downtrend => withRED(s)
        case Flatline => withWHITE(s)
        case Uptrend(_) => withGREEN(s)
      }
    }

    trends.map(colored).mkString(" ")
  }

  def profit(candle: Candle)
            (implicit ctx: TradeContext): String = {
    Option.when(ctx.hasLotsToSell) {
      def colored(amount: Double) = {
        if (amount >= 0) {
          withGREEN("+%-4s%%".format(to2p(amount)))
        } else {
          withRED("%-5s%%".format(to2p(amount)))
        }
      }

      val perc = PriceUtil.calcProfitPerc(candle)

      "C: %-8s/%-8s (%-6s)".format(candle.closingPrice, ctx.minApplicableToSellLotByPrice.price, colored(perc))
    }.getOrElse {
      s"C: ${candle.closingPrice}"
    }
  }

}
