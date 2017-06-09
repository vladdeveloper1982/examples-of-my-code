package com.satrunner.lbs.protocol

import java.util.Locale

import org.joda.time.chrono.ISOChronology
import org.joda.time.{DateTimeZone, DateTime}
import scodec.bits.BitVector
import scodec.{Codec, Err, Attempt, Decoder}
import scodec.codecs._

case class GSMText(text: String, bits: BitVector = BitVector.empty)

object SimpleDecoders {
  private val languagesLetters = Array('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
  private val iso639_1 = Locale.getISOLanguages

  private def char2ToString(chr1: Int, chr2: Int) = languagesLetters(chr1 - 1).toString + languagesLetters(chr2 - 1).toString

  val languageDecoder: Codec[String] = (uint(5) ~~ uint(5)).emap[String] {
    case (firstLetter, secondLetter) if firstLetter > 0 && firstLetter < 27 &&
      secondLetter > 0 && secondLetter < 27 =>
        char2ToString(firstLetter, secondLetter) match {
          case languageCode if iso639_1 contains languageCode => Attempt Successful languageCode
          case languageCode => Attempt.failure(Err(s"Invalid language code: $languageCode ! Language code must be ISO 639-1."))
        }
    case (firstLetter, secondLetter) => Attempt.failure(Err(s"Invalid language letters, first=$firstLetter, second=$secondLetter! Letters must be 1-26."))
  }.econtramap[String]{
    case langCode if iso639_1.contains(langCode.toLowerCase) && langCode.length==2 =>
      Attempt.successful((languagesLetters.indexOf(langCode.toLowerCase.charAt(0)) + 1, languagesLetters.indexOf(langCode.toLowerCase.charAt(1)) + 1))

    case langCode => Attempt.failure(Err(s"Invalid language code $langCode! Language code must be ISO 639-1."))
  }.fuse.withContext("Language codec")

  def isValidTimestamp(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): Boolean = {
    try {
      ISOChronology.getInstanceUTC.getDateTimeMillis(year, month, day, hour, minute, second, 0)
      true
    } catch {
      case e: IllegalArgumentException => false
    }
  }

  val timestampDecoder: Codec[DateTime] = (uint(12) ~~ uint(4) ~~ uint(5) ~~ uint(5) ~~ uint(6) ~~ uint(6)).exmap[DateTime]({

    case (year, month, day, hour, minute, second) if year >= 2010 && year <= 4095 &&
      month >= 1 && month <= 12 &&
      day >= 1 && day <= 31 &&
      hour >= 0 && hour <= 23 &&
      minute >= 0 && minute <= 59 &&
      second >= 0 && second <= 59 &&
      isValidTimestamp(year, month, day, hour, minute, second)
    => Attempt.successful(new DateTime(year, month, day, hour, minute, second, DateTimeZone.UTC))

    case (year, month, day, hour, minute, second) => Attempt.failure(Err(s"Invalid timestamp year=$year, month=$month, day=$day, hour=$hour, minute=$minute, second=$second"))

  }, {

    case v => Attempt.successful((v.getYear, v.getMonthOfYear, v.getDayOfMonth, v.getHourOfDay, v.getMinuteOfHour, v.getSecondOfMinute))

  }).withContext("timestampDecoder")

  val reducedTimestamp: Codec[DateTime] = uint(20).emap[DateTime]({
    case seconds if seconds >= 0 && seconds <= 494100 =>
      val secs = (seconds*64) % 60
      var ost = (seconds*64) / 60
      val minutes = ost % 60
      ost = ost / 60
      val hours = ost % 24
      val days = ost / 24
      Attempt.successful((new DateTime((new DateTime(DateTimeZone.UTC)).getYear, 1, 1, hours, minutes, secs, DateTimeZone.UTC)).plusDays(days-1))

    case seconds => Attempt.failure(Err(s"Invalid reduced timestamp seconds=$seconds"))
  }).decodeOnly.withContext("Reduced Timestamp")

  val latitude: Codec[Double] = (uint(1)~~uint(7)~~uint(6)~~uint(10)).emap{
    case (hemisphere, degrees, minutes, decimalOfMinutes)
      if degrees >= 0 && degrees <= 90 && minutes >= 0 && minutes <= 59 && decimalOfMinutes >= 0 && decimalOfMinutes <= 999 =>
        Attempt.successful( BigDecimal((if (hemisphere == 1) -1.0 else 1.0 ) * (degrees.toDouble + ((minutes.toDouble + decimalOfMinutes * 0.001)/60.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble)

    case (hemisphere, degrees, minutes, decimalOfMinutes) =>
      Attempt.failure(Err(s"Invalid latitude: hemisphere=$hemisphere, degrees=$degrees, minutes=$minutes, decimals=$decimalOfMinutes"))
  }.econtramap[Double]{
    case lat if lat > -90.0 && lat < 90.0 =>
      val degrees = Math.abs(lat).toInt
      val minutes = (Math.abs(lat) - degrees.toInt)*60.0
      val decimalsOfMinutes = (minutes - minutes.toInt)*1000.0
      Attempt.successful((if (lat >= 0) 0 else 1, degrees, minutes.toInt, decimalsOfMinutes.toInt))

    case lat => Attempt.failure(Err(s"Invalid latitude: $lat"))
  }.fuse.withContext("Latitude")

  val longitude: Codec[Double] = (uint(1)~~uint(8)~~uint(6)~~uint(10)).emap{
    case (hemisphere, degrees, minutes, decimalOfMinutes)
      if degrees >= 0 && degrees <= 180 && minutes >= 0 && minutes <= 59 && decimalOfMinutes >= 0 && decimalOfMinutes <= 999 =>
        Attempt.successful(BigDecimal((if (hemisphere == 1) -1.0 else 1.0 ) * (degrees.toDouble + ((minutes.toDouble + decimalOfMinutes * 0.001)/60.0))).setScale(6, BigDecimal.RoundingMode.HALF_UP).toDouble)

    case (hemisphere, degrees, minutes, decimalOfMinutes) =>
      Attempt.failure(Err(s"Invalid longitude: hemisphere=$hemisphere, degrees=$degrees, minutes=$minutes, decimals=$decimalOfMinutes"))
  }.econtramap[Double]{
      case longi if longi > -180.0 && longi < 180.0 =>
        val degrees = Math.abs(longi).toInt
        val minutes = (Math.abs(longi) - degrees.toInt)*60.0
        val decimalsOfMinutes = (minutes - minutes.toInt)*1000.0
        Attempt.successful((if (longi >= 0) 0 else 1, degrees, minutes.toInt, decimalsOfMinutes.toInt))

      case longi => Attempt.failure(Err(s"Invalid longitude: $longi"))
  }.fuse.withContext("Longitude")

  val altitude: Codec[Int] = (uint(1)~~uint(17)).exmap[Int]({
    case (sign, meters) if meters >= 0 && meters <= 99999 => Attempt.successful((if (sign>0) 1 else -1) * meters)
    case (sign, meters) => Attempt.failure(Err(s"Invalid altitude: sign=$sign, meters=$meters"))
  }, {
    case value if value >= -99999 && value <= 99999  => Attempt.successful(((if (value<0) 0 else 1), Math.abs(value)))
    case value => Attempt.failure(Err(s"Invalid altitude: $value"))
  }).fuse.withContext("Altitude")

  def enum[T <: Enumeration](enum: T, length: Int) = uint(length).exmap[T#Value](
    num => enum.values.find(_.id == num).map(Attempt.successful).
      getOrElse(Attempt.failure(Err(s"Invalid enumeration value $num"))) ,
    vl => Attempt.successful(vl.id)
  ).withContext("Enumeration: " + enum.getClass.getName)

  private val lengthGSM: Codec[Int] = choice(constant(BitVector.zero)~~uint(7), constant(BitVector.one)~~uint(15)).xmap[Int]({
    case (_, size) => (((size * 7) / 8).toInt*8 + (if ((size * 7) % 8 > 0) 1 else 0) * 8)
  }, {
    case v => (Unit, v / 7)
  }).withContext("GSM Text Length")

  private val lengthASCII: Codec[Int] = choice(constant(BitVector.zero)~~uint(7), constant(BitVector.one)~~uint(15)).xmap[Int]({
    case (_, size) => size * 8
  }, {
    case v => (Unit, v / 8)
  }).withContext("ASCII Text Length")

  val gsmText = variableSizeBits(lengthGSM, bits).xmap[GSMText](bits => GSMText(TextUtils.bitsToGsm(bits), bits), txt => TextUtils.gsmToBits(txt.text))

  val asciiText = variableSizeBits(lengthASCII, ascii)

  def rangedUInt(size: Int, min: Int, max: Int) = uint(size).exmap[Int]({
    case v if v >= min && v <= max => Attempt.successful(v)
    case v => Attempt.failure(Err(s"Invalid value $v, value must be from $min and $max"))
  }, {
    case v if v >= min && v <= max => Attempt.successful(v)
    case v => Attempt.failure(Err(s"Invalid value $v, value must be from $min and $max"))
  }).fuse
}
