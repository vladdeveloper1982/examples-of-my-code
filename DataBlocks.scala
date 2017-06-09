package com.satrunner.lbs.protocol

import org.joda.time.DateTime
import com.satrunner.lbs.protocol.DataBlockType.DataBlockType
import com.satrunner.lbs.protocol.DestinationType.DestinationType
import com.satrunner.lbs.protocol.GeofenceStatus.GeofenceStatus
import com.satrunner.lbs.protocol.MeasurementUnits.MeasurementUnits
import com.satrunner.lbs.protocol.ChargerStatus.ChargerStatus
import com.satrunner.lbs.protocol.MessageEncodingType.MessageEncodingType
import com.satrunner.lbs.protocol.PortalStatus.PortalStatus
import com.satrunner.lbs.protocol.RateUnits.RateUnits
import com.satrunner.lbs.protocol.SourceLocation.SourceLocation
import com.satrunner.lbs.protocol.SourceOfFix.SourceOfFix
import com.satrunner.lbs.protocol.SynchronisationType.SynchronisationType
import com.satrunner.lbs.protocol.VelocityDirection.VelocityDirection
import scodec.{Attempt, Err, Codec, DecodeResult, SizeBound}
import scodec.bits.BitVector
import scodec.codecs._
import SimpleDecoders._

trait DataBlock {
  def blockType: DataBlockType //todo: yes it is calculated
}

trait DataBlockCodec[A<:DataBlock] {
  val codec: Codec[A]
}

object DataBlock {
  val codecs = Map[DataBlockType, DataBlockCodec[_]](
    DataBlockType.location -> LocationBlock,
    DataBlockType.freeText -> FreeTextBlock,
    DataBlockType.recipient -> RecipientBlock,
    DataBlockType.synchronisation -> SynchronisationBlock,
    DataBlockType.deviceIdentifier -> DeviceIdentifierBlock,
    DataBlockType.mobilePhoneDiagnostics -> MobilePhoneDiagnosticsBlock,
    DataBlockType.sender -> SenderBlock,
    DataBlockType.regionCircle -> RegionCircleBlock,
    DataBlockType.regionPolygon -> RegionPolygonBlock,
    DataBlockType.portalStatus -> PortalStatusBlock,
    DataBlockType.phoneSettings -> PhoneSettingsBlock,
    DataBlockType.geofenceStatus -> GeofenceStatusBlock,
    DataBlockType.cannedMessagesHash -> CannedMessagesHashBlock,
    DataBlockType.portalProviderInformation -> PortalProviderInformationBlock,
    DataBlockType.reducedSizeLocation -> ReducedLocationBlock,
    DataBlockType.cannedMessageIdentifier -> CannedMessageIdentifierBlock,
    DataBlockType.distributionListHash -> DistributionListHashBlock,
    DataBlockType.messageIdentifier -> MessageIdentifierBlock,
    DataBlockType.firmwareHardwareVersion -> FirmwareHardwareVersionBlock
  )

  private val lengthBlock: Codec[Int] = choice(constant(BitVector.zero)~~uint(7), constant(BitVector.one)~~uint(15)).xmap[Int]({
    case (_, size) => size
  }, {
    case v => (Unit, v)
  }).withContext("Data Block Length")

  val oneBlock = (DataBlockType.codec(8)~~variableSizeBytes(lengthBlock, bits)).exmap[DataBlock]({
    case (blockType, data) => codecs.get(blockType).get.codec.asInstanceOf[Codec[DataBlock]].decode(data).map(_.value)
  }, {
    case dataBlock => codecs.get(dataBlock.blockType).
      map(d => d.codec.asInstanceOf[Codec[DataBlock]].encode(dataBlock).map(p => (dataBlock.blockType, p.toByteVector.toBitVector))).
        getOrElse(Attempt.failure(Err(s"Invalid block type ${dataBlock.blockType}")))
  }).withContext("Data Block")

  val listBlocks = list(oneBlock)
}

case class Accuracy(units: MeasurementUnits, number: Int)

case object Accuracy {
  val codec: Codec[Accuracy] = (MeasurementUnits.codec(1)~~uint(14)).
    exmap[Accuracy]({
      case (units, number) if number>=0 && number<=15000 => Attempt.successful(Accuracy.apply(units, number))
      case (units, number) => Attempt.failure(Err(s"Invalid accuracy number=$number and units=$units"))
    },{
      case accuracy if accuracy.number >= 0 && accuracy.number <= 15000 => Attempt.successful(Accuracy.unapply(accuracy).get)
      case accuracy => Attempt.failure(Err(s"Invalid accuracy number ${accuracy.number}"))
    }).withContext("Accuracy")
}

case class LocationBlock(isLong: Boolean, // location block is long, if false then reduced
                         /* base fields in all types blocks*/
                         latitude: Double,
                         longitude: Double,
                         isValid: Boolean,
                         /* for Reduced Location block */
                         sourceOfFix: Option[SourceOfFix] = None,
                         ageOfFix: Option[Int] = None,
                         /* for Long Location block */
                         speed: Option[Int] = None,
                         velocityDirection: Option[VelocityDirection] = None,
                         course: Option[Int] = None,
                         altitude: Option[Int] = None,
                         source: Option[SourceLocation] = None,
                         latitudeAccuracy: Option[Accuracy] = None,
                         longitudeAccuracy: Option[Accuracy] = None,
                         altitudeAccuracy: Option[Accuracy] = None,
                         gpsTimestamp: Option[DateTime] = None) extends DataBlock {
  def blockType = if (isLong) DataBlockType.location else DataBlockType.reducedSizeLocation
}

case object LocationBlock extends DataBlockCodec[LocationBlock] {
  val codec: Codec[LocationBlock] = (latitude~~longitude~~uint(10)~~VelocityDirection.codec(2)~~uint(9)~~altitude~~
    SourceLocation.codec(3)~~Accuracy.codec~~Accuracy.codec~~Accuracy.codec~~bool~~timestampDecoder).dropRight(ignore(1)).exmap[LocationBlock]({
// todo alex: look at Idea tips, eg it suggests to use name boolean vals(isLong = true instead true) and says about shadowing altitude
    // todo alex: reduce longitude only either lon or long, and not longi
    case (lat, lon, speed, direction, course, altitude, source, latAccuracy, lonAccuracy, altAccuracy,
      isValid, gpsTimestamp)  => Attempt.successful(LocationBlock(isLong = true, lat, lon, isValid = isValid, None, None, Some(speed),
        Some(direction), Some(course), Some(altitude), Some(source), Some(latAccuracy), Some(lonAccuracy), Some(altAccuracy), Some(gpsTimestamp)))

  }, {

    case loc => Attempt.successful((loc.latitude, loc.longitude, loc.speed.get, loc.velocityDirection.get,
      loc.course.get, loc.altitude.get, loc.source.get, loc.latitudeAccuracy.get, loc.longitudeAccuracy.get,
      loc.altitudeAccuracy.get, loc.isValid, loc.gpsTimestamp.get))

  }).withContext("Location Block")
}

case object ReducedLocationBlock extends DataBlockCodec[LocationBlock] {

  def apply(latitude: Double, longitude: Double, isValid: Boolean, sourceOfFix: SourceOfFix, ageOfGPS: Int) =
    LocationBlock(isLong=false, latitude=latitude, longitude=longitude, isValid=isValid, sourceOfFix=Some(sourceOfFix), ageOfFix=Some(ageOfGPS))

  def unapply(loc: LocationBlock) = (loc.latitude, loc.longitude, loc.isLong, loc.sourceOfFix.get, loc.ageOfFix.get)

  val codec = (latitude~~longitude~~bool~~SourceOfFix.codec(1)~~uint(5)).
    xmap[LocationBlock]((ReducedLocationBlock.apply _).tupled, unapply _).withContext("Reduced Location Block")
}

case class FreeTextBlock(text: String) extends DataBlock {
  def blockType = DataBlockType.freeText
}

case object FreeTextBlock extends DataBlockCodec[FreeTextBlock] {
  val codec = gsmText.xmap[FreeTextBlock](b => FreeTextBlock(b.text), b => GSMText(b.text, BitVector.empty)).withContext("Free Text Block")
}

case class RecipientBlock(text: String) extends DataBlock {
  def blockType = DataBlockType.recipient
}

case object RecipientBlock extends DataBlockCodec[RecipientBlock] {
  val codec = gsmText.xmap[RecipientBlock](b => RecipientBlock(b.text), b => GSMText(b.text, BitVector.empty)).withContext("Recipioent Block")
}

trait SynchronisationItem {
  val itemType: SynchronisationType
}

case class SyncAddressBookEntry(number: Int, item: String) extends SynchronisationItem {
  val itemType = SynchronisationType.AddressBookEntry
}

case object SyncAddressBookEntry {
  val codec = (uint8~~gsmText).xmap[SyncAddressBookEntry](d => SyncAddressBookEntry(d._1, d._2.text), d => (d.number, GSMText(d.item))).
    withContext("Synchronisaction Address Book Entry")
}

case class SyncCannedMessage(language: String, messageNumber: Int, includeLocation: Boolean, cannedMessage: String) extends SynchronisationItem {
  val itemType = SynchronisationType.CannedMessage
}

case object SyncCannedMessage {
  val codec = (languageDecoder~~uint8~~bool~~ignore(5)~~gsmText).xmap[SyncCannedMessage]({
    case (language, num, includeLocation, _, text) => SyncCannedMessage(language, num, includeLocation, text.text)
  }, v => (v.language, v.messageNumber, v.includeLocation, Unit, GSMText(v.cannedMessage))).withContext("Synchronisation Canned Message")
}

case class SyncGPSFrequencyRate(reportingRate: Int, rateUnits: RateUnits) extends SynchronisationItem {
  val itemType = SynchronisationType.GPSFrequencyRate
}

case object SyncGPSFrequencyRate {
  val codec = (uint(6)~~RateUnits.codec(3)~~ignore(7)).emap{
    case (value, units, _) if value >= 0 && value <= 59 => Attempt.successful(SyncGPSFrequencyRate(value, units))
    case (value, units, _) => Attempt.failure(Err(s"Invalid Frequency Rate: rate=$value, units=$units"))
  }.contramap[SyncGPSFrequencyRate](v => (v.reportingRate, v.rateUnits, Unit)).fuse.withContext("Synchronisation GPS Frequency Rate")
}

case class SyncEmergencyDestination(emergencyDestination: DestinationType) extends SynchronisationItem {
  val itemType = SynchronisationType.EmergencyDestination
}

case object SyncEmergencyDestination {
  val codec = (DestinationType.codec(2)~~ignore(6)).xmap[SyncEmergencyDestination](d => SyncEmergencyDestination(d._1),
    d => (d.emergencyDestination, Unit)).withContext("Synchronisation Emergency Destination")
}

case class SyncEmergencyCallRecipient(recipient: String) extends SynchronisationItem {
  val itemType = SynchronisationType.EmergencyCallRecipient
}

case object SyncEmergencyCallRecipient {
  val codec = gsmText.xmap[SyncEmergencyCallRecipient](txt => SyncEmergencyCallRecipient(txt.text), s => (GSMText(s.recipient))).
    withContext("Synchronisation Emergency Call Recipient")
}

case class SyncProtocolType(protocolType: MessageEncodingType) extends SynchronisationItem {
  val itemType = SynchronisationType.ProtocolType
}

case object SyncProtocolType {
  val codec = (MessageEncodingType.codec(2)~~ignore(6)).xmap[SyncProtocolType](d => SyncProtocolType(d._1),
    d => (d.protocolType, Unit)).withContext("Synchronisation Protocol Type")
}

case class SyncDistributionListAdd(distributionList: String) extends SynchronisationItem {
  val itemType = SynchronisationType.DistributionListAdd
}

case object SyncDistributionListAdd {
  val codec = gsmText.xmap[SyncDistributionListAdd](txt => SyncDistributionListAdd(txt.text), s => (GSMText(s.distributionList))).
    withContext("Synchronisation Distribution List Add")
}

case class SyncDistributionListClear(distributionList: String) extends SynchronisationItem {
  val itemType = SynchronisationType.DistributionListClear
}

case object SyncDistributionListClear {
  val codec = gsmText.xmap[SyncDistributionListClear](txt => SyncDistributionListClear(txt.text), s => (GSMText(s.distributionList))).
    withContext("Synchronisation Distribution List Clear")
}

case class SyncDistributionListClearAll() extends SynchronisationItem {
  val itemType = SynchronisationType.DistributionListClearAll
}

case class SynchronisationBlock(item: SynchronisationItem) extends DataBlock {
  def blockType = DataBlockType.synchronisation
}

case object SynchronisationBlock extends DataBlockCodec[SynchronisationBlock] {

  def doCode[A<:SynchronisationItem](c: Codec[A], item: SynchronisationItem) = c.encode(item.asInstanceOf[A]).map(p => (item.itemType, p))

  def doDecode[A<:SynchronisationItem](d: Codec[A], data: BitVector) = d.decode(data).map(p => SynchronisationBlock(p.value))

  val codec = (SynchronisationType.codec(8)~~bits).exmap[SynchronisationBlock]({
    case (SynchronisationType.AddressBookEntry, data) => doDecode(SyncAddressBookEntry.codec, data)
    case (SynchronisationType.CannedMessage, data) => doDecode(SyncCannedMessage.codec, data)
    case (SynchronisationType.GPSFrequencyRate, data) => doDecode(SyncGPSFrequencyRate.codec, data)
    case (SynchronisationType.EmergencyDestination, data) => doDecode(SyncEmergencyDestination.codec, data)
    case (SynchronisationType.EmergencyCallRecipient, data) => doDecode(SyncEmergencyCallRecipient.codec, data)
    case (SynchronisationType.ProtocolType, data) => doDecode(SyncProtocolType.codec, data)
    case (SynchronisationType.DistributionListAdd, data) => doDecode(SyncDistributionListAdd.codec, data)
    case (SynchronisationType.DistributionListClear, data) => doDecode(SyncDistributionListClear.codec, data)
    case (SynchronisationType.DistributionListClearAll, data) => Attempt.successful(SynchronisationBlock(SyncDistributionListClearAll()))
    case (_, data) => Attempt.failure(Err("Invalid synchronisation block with item=" + data.toHex))
  }, item => item.item.itemType match {
    case SynchronisationType.AddressBookEntry => doCode(SyncAddressBookEntry.codec, item.item)
    case SynchronisationType.CannedMessage => doCode(SyncCannedMessage.codec, item.item)
    case SynchronisationType.GPSFrequencyRate => doCode(SyncGPSFrequencyRate.codec, item.item)
    case SynchronisationType.EmergencyDestination => doCode(SyncEmergencyDestination.codec, item.item)
    case SynchronisationType.EmergencyCallRecipient => doCode(SyncEmergencyCallRecipient.codec, item.item)
    case SynchronisationType.ProtocolType => doCode(SyncProtocolType.codec, item.item)
    case SynchronisationType.DistributionListAdd => doCode(SyncDistributionListAdd.codec, item.item)
    case SynchronisationType.DistributionListClear => doCode(SyncDistributionListClear.codec, item.item)
    case SynchronisationType.DistributionListClearAll => Attempt.successful((SynchronisationType.DistributionListClearAll, BitVector.empty))
  }).fuse.withContext("Synchronisation Block")
}

case class DeviceIdentifierBlock(deviceId: String) extends DataBlock {
  def blockType = DataBlockType.deviceIdentifier
}

case object DeviceIdentifierBlock extends DataBlockCodec[DeviceIdentifierBlock] {
  val codec = asciiText.xmap[DeviceIdentifierBlock](DeviceIdentifierBlock(_), (_.deviceId)).fuse.withContext("Device Identifier Block")
}

case class MobilePhoneDiagnosticsBlock(batteryVoltagy: Int,
                                       chargerStatus: ChargerStatus,
                                       accumulatorRange: Int,
                                       numVisibleGPSSatellites: Int,
                                       firmwareHardwareVersion: String) extends DataBlock {
  def blockType = DataBlockType.mobilePhoneDiagnostics
}

case object MobilePhoneDiagnosticsBlock extends DataBlockCodec[MobilePhoneDiagnosticsBlock] {
  val batteryVoltage = rangedUInt(6, 0, 63).withContext("Battery Voltage")
  val accumulatorRange = rangedUInt(8, 0, 100).withContext("Accumulator range")

  val codec = (batteryVoltage~~ChargerStatus.codec(2)~~accumulatorRange~~uint8~~asciiText).
    xmap[MobilePhoneDiagnosticsBlock]((MobilePhoneDiagnosticsBlock.apply _).tupled, MobilePhoneDiagnosticsBlock.unapply(_).get).
    fuse.withContext("Mobile Phone Diagnostics Block")
}

case class SenderBlock(sender: String) extends DataBlock {
  def blockType = DataBlockType.sender
}

case object SenderBlock extends DataBlockCodec[SenderBlock] {
  val codec = gsmText.xmap[SenderBlock](p => SenderBlock(p.text), p => GSMText(p.sender)).withContext("Sender Block")
}

case class Fence(units: MeasurementUnits, value: Int)

case object Fence {
  val codec = (MeasurementUnits.codec(1)~~rangedUInt(13, 0, 8191)).
    xmap[Fence]((Fence.apply _).tupled, Fence.unapply(_).get).withContext("Fence")
}

case class RegionCircleBlock(alertWhenCrossed: Boolean,
                              fenceHysteresis: Fence,
                              fenceProximity: Fence,
                              refreshRate: Int, /* in minutes*/
                              latitude: Double,
                              longitude: Double,
                              radiusLength: Fence) extends DataBlock {
  def blockType = DataBlockType.regionCircle
}

case object RegionCircleBlock extends DataBlockCodec[RegionCircleBlock] {
  val refreshRate = rangedUInt(11, 0, 2048)

  val codec = (bool~~Fence.codec~~Fence.codec~~refreshRate~~latitude~~longitude~~Fence.codec~~ignore(1)).xmap[RegionCircleBlock]({
      case (alert, hist, proximity, rate, lat, longi, radius, _) =>
        RegionCircleBlock(alert, hist, proximity, rate, lat, longi, radius)
    }, p => (p.alertWhenCrossed, p.fenceHysteresis, p.fenceProximity, p.refreshRate, p.latitude, p.longitude,
      p.radiusLength, Unit)).withContext("Region Circle Block")
}

case class RegionPolygonPoint(latitude: Double, longitude: Double)

case object RegionPolygonPoint {
  val codec = (latitude~~longitude).
    xmap[RegionPolygonPoint]((RegionPolygonPoint.apply _), RegionPolygonPoint.unapply(_).get).withContext("Polygon Point")
}

case class RegionPolygonBlock(alertWhenCrossed: Boolean,
                              fenceHysteresis: Fence,
                              fenceProximity: Fence,
                              refreshRate: Int, /* in minutes*/
                              polygon: List[RegionPolygonPoint]) extends DataBlock {
  def blockType = DataBlockType.regionPolygon
}

object ListPointCodec extends Codec[List[RegionPolygonPoint]] {

  override def encode(list: List[RegionPolygonPoint]): Attempt[BitVector] =
    if (list.size >= 3 && list.size <= 8) {
      val result = list.map(RegionPolygonPoint.codec.encode)
      if (result.forall(_.isSuccessful)) {
        val data = result.map(_.require).foldLeft(BitVector.empty)((acc, v) => acc++v)
        Attempt.successful((if (data.size % 8 > 0) data.padRight((data.size/8 + 1)*8) else data))
      }
      else
        result.find(_.isFailure).get
    }
    else {
      Attempt.failure(Err(s"Invalid size list of polygon points! size list=${list.size}"))
    }

  override def sizeBound: SizeBound = SizeBound.unknown

  override def decode(bts: BitVector): Attempt[DecodeResult[List[RegionPolygonPoint]]] = {
    val nsize = (bts.size / 49).toInt

    if (nsize < 3 || nsize > 8) {
      Attempt.failure(Err("Invalid size list of polygon points! size must be between 3 and 8"))
    } else {
      val result = (1 to nsize).map(i => RegionPolygonPoint.codec.decode(bts.slice((i-1)*49, i*49)))
      if (result.forall(_.isSuccessful)) {
        Attempt.successful(DecodeResult(result.map(_.require.value).toList, bts))
      } else {
        result.find(_.isFailure).get.asInstanceOf[Attempt[DecodeResult[List[RegionPolygonPoint]]]]
      }
    }
  }
}

case object RegionPolygonBlock extends DataBlockCodec[RegionPolygonBlock] {
  val refreshRate = rangedUInt(11, 0, 2048)

  val codec = (bool~~Fence.codec~~Fence.codec~~refreshRate~~ListPointCodec).
    xmap[RegionPolygonBlock]((RegionPolygonBlock.apply _).tupled, RegionPolygonBlock.unapply(_).get).withContext("Region Polygon Block")
}

case class PortalStatusBlock(status: PortalStatus, statusText: String) extends DataBlock {
  def blockType = DataBlockType.portalStatus
}

case object PortalStatusBlock extends DataBlockCodec[PortalStatusBlock] {
  val codec = (PortalStatus.codec(2)~~ignore(6)~~gsmText).xmap[PortalStatusBlock](
    p => PortalStatusBlock(p._1, p._3.text), p => (p.status, Unit, GSMText(p.statusText))).withContext("Portal Status Block")
}

case class PhoneSettingsBlock(reportingFrequencyRate: Int,
                              reportingFrequencyRateUnits: RateUnits,
                              emergencyDestination: DestinationType,
                              messageEncoding: MessageEncodingType,
                              emergencyCallRecipient: String
                              ) extends DataBlock {
  def blockType = DataBlockType.phoneSettings
}

case object PhoneSettingsBlock extends DataBlockCodec[PhoneSettingsBlock] {
  val reporting = rangedUInt(6, 0, 59).withContext("Reporting Frequency Rate")

  val codec = (reporting~~RateUnits.codec(3)~~DestinationType.codec(2)~~MessageEncodingType.codec(2)~~ignore(3)~~gsmText).
    xmap[PhoneSettingsBlock](p => PhoneSettingsBlock(p._1, p._2, p._3, p._4, p._6.text),
      p => (p.reportingFrequencyRate, p.reportingFrequencyRateUnits, p.emergencyDestination, p.messageEncoding,
          Unit, GSMText(p.emergencyCallRecipient))).withContext("Phone Settings Block")
}

case class GeofenceStatusBlock(status: GeofenceStatus) extends DataBlock {
  def blockType = DataBlockType.geofenceStatus
}

case object GeofenceStatusBlock extends DataBlockCodec[GeofenceStatusBlock] {
  val codec = (GeofenceStatus.codec(1)~~ignore(7)).xmap[GeofenceStatusBlock](
    p => GeofenceStatusBlock(p._1), p => (p.status, Unit)).withContext("Geofence Status Block")
}

case class CannedMessagesHashBlock(hash: String) extends DataBlock {
  def blockType = DataBlockType.cannedMessagesHash
}

case object CannedMessagesHashBlock extends DataBlockCodec[CannedMessagesHashBlock] {
  val codec = bits(128).xmap[CannedMessagesHashBlock](p => CannedMessagesHashBlock(p.toHex),
    p => (BitVector.fromHex(p.hash).get)).withContext("Canned Message Hash Block")
}

case class PortalProviderInformationBlock(text: String) extends DataBlock {
  def blockType = DataBlockType.portalProviderInformation
}

case object PortalProviderInformationBlock extends DataBlockCodec[PortalProviderInformationBlock] {
  val codec = gsmText.xmap[PortalProviderInformationBlock](
    p => PortalProviderInformationBlock.apply(p.text), PortalProviderInformationBlock.unapply(_).map(GSMText(_)).get)
}

case class CannedMessageIdentifierBlock(messageNumber: Int, crc: String) extends DataBlock {
  def blockType = DataBlockType.cannedMessageIdentifier
}

case object CannedMessageIdentifierBlock extends DataBlockCodec[CannedMessageIdentifierBlock] {
  val number = rangedUInt(8, 1, 30).withContext("Message Number")

  val codec = (number~~bits(32)).xmap[CannedMessageIdentifierBlock](
    p => CannedMessageIdentifierBlock(p._1, p._2.toHex), p => (p.messageNumber, BitVector.fromHex(p.crc).get)).withContext("Canned Message Identifier Block")
}

case class DistributionListHashBlock(hash: String) extends DataBlock {
  def blockType = DataBlockType.distributionListHash
}

case object DistributionListHashBlock extends DataBlockCodec[DistributionListHashBlock] {
  val codec = bits(128).xmap[DistributionListHashBlock](p => DistributionListHashBlock(p.toHex),
    p => (BitVector.fromHex(p.hash).get)).withContext("Distribution List Hash Block")
}

case class MessageIdentifierBlock(messageIdentifier: Int) extends DataBlock {
  def blockType = DataBlockType.messageIdentifier
}

case object MessageIdentifierBlock extends DataBlockCodec[MessageIdentifierBlock] {
  val codec = rangedUInt(8, 1, 255).xmap[MessageIdentifierBlock](MessageIdentifierBlock.apply _, _.messageIdentifier).
    withContext("Message Identifier Block")
}

case class FirmwareHardwareVersionBlock(versionText: String) extends DataBlock {
  def blockType = DataBlockType.firmwareHardwareVersion
}

case object FirmwareHardwareVersionBlock extends DataBlockCodec[FirmwareHardwareVersionBlock] {
  val codec = asciiText.xmap[FirmwareHardwareVersionBlock](FirmwareHardwareVersionBlock.apply _, _.versionText).
    withContext("Firmware and Hardware Version Block")
}