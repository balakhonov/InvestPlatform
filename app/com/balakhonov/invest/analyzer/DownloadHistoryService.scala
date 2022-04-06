package com.balakhonov.invest.analyzer

import com.balakhonov.invest.dao.UtilDAO
import com.balakhonov.invest.models.enums.CandleTimeFrame.CandleTimeFrame
import com.balakhonov.invest.provider.ClientProvider

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits
import scala.concurrent.Future

@Singleton
class DownloadHistoryService @Inject()(utilDAO: UtilDAO) {

  def downloadByYear(figi: String,
                     year: Int,
                     timeFrame: CandleTimeFrame)
                    (implicit provider: ClientProvider): Future[Unit] = {
    print(s"[$figi] Year=$year TimeFrame=$timeFrame Downloading.. ")
    val resultF = provider.pullHistoryByYear(figi, year, timeFrame)

    resultF.map { list =>
      utilDAO.candleDao.insert(list: _ *)
      print(s"[$figi] Year=$year TimeFrame=$timeFrame Done. size=" + list.size + " ... \n")
    }(Implicits.global)
  }

  def downloadByYear(figies: Seq[String],
                     year: Int,
                     timeFrame: CandleTimeFrame)
                    (implicit provider: ClientProvider): Future[Unit] = {
    figies.foldLeft[Future[Unit]](Future.successful(())) { case (acc, figi) =>
      acc.flatMap { _ =>
        Thread.sleep(1000)

        downloadByYear(figi, year, timeFrame)
          .map(_ => println(s"Yearly($year) period successfully downloaded for $figi"))(Implicits.global)
          .recoverWith {
            case ex =>
              throw new Exception(s"FAILED while Pulling $figi", ex)
          }(Implicits.global)
      }(Implicits.global)
    }
  }

  def downloaByMonth(figi: String,
                     year: Int,
                     month: Int,
                     timeFrame: CandleTimeFrame)
                    (implicit provider: ClientProvider): Future[Unit] = {
    print(s"[$figi] Downloading.. ")
    val resultF = provider.pullHistoryByMonth(figi, year, month, timeFrame)

    resultF.map { list =>
      utilDAO.candleDao.insert(list: _ *)
      print(s"[$figi] $month-$year Done. size=" + list.size + " ... \n")
    }(Implicits.global)
  }
}
