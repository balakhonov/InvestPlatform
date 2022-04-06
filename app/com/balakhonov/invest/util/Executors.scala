package com.balakhonov.invest.util

import java.util.concurrent.{LinkedBlockingQueue, ThreadPoolExecutor, TimeUnit}

object Executors {

  def newFixedThreadPool(prefix: String,
                         threads: Int): ThreadPoolExecutor = {
    val threadFactory = new NamedThreadFactory(prefix)
    new ThreadPoolExecutor(threads, threads, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue[Runnable], threadFactory)
  }

}