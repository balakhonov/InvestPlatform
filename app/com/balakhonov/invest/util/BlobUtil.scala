package com.balakhonov.invest.util

import java.util.Base64

object BlobUtil {

  def encode(blob: Array[Byte]): String = Base64.getEncoder.encodeToString(blob)

  def decode(str: String): Array[Byte] = Base64.getDecoder.decode(str)

}
