package utils

import org.apache.commons.lang3.StringUtils
import scodec.bits.{ByteOrdering, BitVector}

/**
 * Created by Vladislav Gargalyk.
 * Company: Satrunner
 */
object ImeiHash2 {

  def hash1(imei: String): String = StringUtils.leftPad(
    imei.foldLeft(BitVector.fromInt(0, 16)){ (acc, c) =>

      acc ^ BitVector.fromInt((acc << 5).toInt(false) + (acc >>> 2).toInt(false) + c.toInt, 16)
    }.toInt(false).toString, 5, "0")

  def hash2(imei: String, hash1: String): String = StringUtils.leftPad(
    imei.reverse.foldLeft(BitVector.fromInt(hash1.toInt, 16)){ (acc, c) =>

      acc ^ BitVector.fromInt((acc << 5).toInt(false) + (acc >>> 2).toInt(false) + c.toInt, 16)
    }.toInt(false).toString, 5, "0")
}