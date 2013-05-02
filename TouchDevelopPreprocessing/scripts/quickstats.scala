/**
 * Created with IntelliJ IDEA.
 * User: lucas
 * Date: 4/26/13
 * Time: 4:14 PM
 * To change this template use File | Settings | File Templates.
 */


var curScript = ""
var isMissingSemantics = false
var numberAlarmsInvalid = 0
var numberAlarmsNetwork = 0
var numberAlarmsBounds = 0
var numberAlarmsOthers = 0
var numberDummyMethods = 0
var analysisFailure = false

for( ln <- io.Source.fromFile("TouchDevelopPreprocessing/scripts/quickstats.txt").getLines ) {

  val Start = """\{  ANALYZING ♻([\w]+).*""".r
  val End = """\}  \(time: ([0-9\.]*) s\)""".r
  val Bottom = """BOTTOM: State is bottom .*""".r
  val Precision1 = """PRECISION: Could not find .*""".r
  val Precision2 = """PRECISION: .* not implemented.*""".r
  val AlarmInvalid = """ALARM: .* might be invalid .*""".r
  val AlarmNetwork = """ALARM: .* device is connected .*""".r
  val AlarmBounds1 = """ALARM: .* may be less than the lowest allowed value .*""".r
  val AlarmBounds2 = """ALARM: .* may be greater than the highest allowed value .*""".r
  val AlarmBounds3 = """ALARM: .* may be too small .*""".r
  val AlarmBounds4 = """ALARM: .* might be negative .*""".r
  val DummyMethod1 = """PRECISION: .* is a dummy at .*""".r
  val DummyMethod2 = """Unhandled expression type in APRON interface.*""".r
  val OtherAlarm = """ALARM: .*""".r
  val AnalysisFailure = """ANALYSIS ERROR: .*""".r

  ln match {
    case Start(script) =>
      curScript = script
    case Bottom() =>
      isMissingSemantics = true
    case Precision1() =>
      isMissingSemantics = true
    case Precision2() =>
      isMissingSemantics = true
    case End(time) =>
      println(curScript+"\t"+time.replace(".",",")+"\t"+isMissingSemantics+"\t"+numberAlarmsInvalid+"\t"+numberAlarmsNetwork+"\t"+numberAlarmsBounds+"\t"+numberDummyMethods+"\t"+numberAlarmsOthers+"\t"+analysisFailure)
      isMissingSemantics = false
      analysisFailure = false
      numberAlarmsInvalid = 0
      numberAlarmsNetwork = 0
      numberAlarmsBounds = 0
      numberAlarmsOthers = 0
      numberDummyMethods = 0
    case AlarmInvalid() =>
      numberAlarmsInvalid = numberAlarmsInvalid + 1
    case AlarmNetwork() =>
      numberAlarmsNetwork = numberAlarmsNetwork + 1
    case AlarmBounds1() =>
      numberAlarmsBounds = numberAlarmsBounds + 1
    case AlarmBounds2() =>
      numberAlarmsBounds = numberAlarmsBounds + 1
    case AlarmBounds3() =>
      numberAlarmsBounds = numberAlarmsBounds + 1
    case AlarmBounds4() =>
      numberAlarmsBounds = numberAlarmsBounds + 1
    case DummyMethod1() =>
      numberDummyMethods = numberDummyMethods + 1
    case DummyMethod2() =>
      numberDummyMethods = numberDummyMethods + 1
    case OtherAlarm() =>
      numberAlarmsOthers = numberAlarmsOthers + 1
    case AnalysisFailure() =>
      analysisFailure = true
    case _ =>

  }

}