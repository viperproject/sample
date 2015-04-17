package ch.ethz.inf.pm.sample.wala

import org.scalatest.FunSuite

/**
 * @author Lucas Brutschy
 */
class ScanDroidEngineTest extends FunSuite {

  val tests =
    """Aliasing/Merge1.apk
      |AndroidSpecific/ApplicationModeling1.apk
      |AndroidSpecific/DirectLeak1.apk
      |AndroidSpecific/InactiveActivity.apk
      |AndroidSpecific/Library2.apk
      |AndroidSpecific/LogNoLeak.apk
      |AndroidSpecific/Obfuscation1.apk
      |AndroidSpecific/Parcel1.apk
      |AndroidSpecific/PrivateDataLeak1.apk
      |AndroidSpecific/PrivateDataLeak2.apk
      |AndroidSpecific/PrivateDataLeak3.apk
      |AndroidSpecific/PublicAPIField1.apk
      |AndroidSpecific/PublicAPIField2.apk
      |ArraysAndLists/ArrayAccess1.apk
      |ArraysAndLists/ArrayAccess2.apk
      |ArraysAndLists/ArrayCopy1.apk
      |ArraysAndLists/ArrayToString1.apk
      |ArraysAndLists/HashMapAccess1.apk
      |ArraysAndLists/ListAccess1.apk
      |ArraysAndLists/MultidimensionalArray1.apk
      |Callbacks/AnonymousClass1.apk
      |Callbacks/Button1.apk
      |Callbacks/Button2.apk
      |Callbacks/Button3.apk
      |Callbacks/Button4.apk
      |Callbacks/Button5.apk
      |Callbacks/LocationLeak1.apk
      |Callbacks/LocationLeak2.apk
      |Callbacks/LocationLeak3.apk
      |Callbacks/MethodOverride1.apk
      |Callbacks/MultiHandlers1.apk
      |Callbacks/Ordering1.apk
      |Callbacks/RegisterGlobal1.apk
      |Callbacks/RegisterGlobal2.apk
      |Callbacks/Unregister1.apk
      |EmulatorDetection/ContentProvider1.apk
      |EmulatorDetection/IMEI1.apk
      |EmulatorDetection/PlayStore1.apk
      |FieldAndObjectSensitivity/FieldSensitivity1.apk
      |FieldAndObjectSensitivity/FieldSensitivity2.apk
      |FieldAndObjectSensitivity/FieldSensitivity3.apk
      |FieldAndObjectSensitivity/FieldSensitivity4.apk
      |FieldAndObjectSensitivity/InheritedObjects1.apk
      |FieldAndObjectSensitivity/ObjectSensitivity1.apk
      |FieldAndObjectSensitivity/ObjectSensitivity2.apk
      |GeneralJava/Clone1.apk
      |GeneralJava/Exceptions1.apk
      |GeneralJava/Exceptions2.apk
      |GeneralJava/Exceptions3.apk
      |GeneralJava/Exceptions4.apk
      |GeneralJava/FactoryMethods1.apk
      |GeneralJava/Loop1.apk
      |GeneralJava/Loop2.apk
      |GeneralJava/Serialization1.apk
      |GeneralJava/SourceCodeSpecific1.apk
      |GeneralJava/StartProcessWithSecret1.apk
      |GeneralJava/StaticInitialization1.apk
      |GeneralJava/StaticInitialization2.apk
      |GeneralJava/StaticInitialization3.apk
      |GeneralJava/StringFormatter1.apk
      |GeneralJava/StringPatternMatching1.apk
      |GeneralJava/StringToCharArray1.apk
      |GeneralJava/StringToOutputStream1.apk
      |GeneralJava/UnreachableCode.apk
      |GeneralJava/VirtualDispatch1.apk
      |GeneralJava/VirtualDispatch2.apk
      |GeneralJava/VirtualDispatch3.apk
      |GeneralJava/VirtualDispatch4.apk
      |ImplicitFlows/ImplicitFlow1.apk
      |ImplicitFlows/ImplicitFlow2.apk
      |ImplicitFlows/ImplicitFlow3.apk
      |ImplicitFlows/ImplicitFlow4.apk
      |InterAppCommunication/Echoer.apk
      |InterAppCommunication/SendSMS.apk
      |InterAppCommunication/StartActivityForResult1.apk
      |InterComponentCommunication/ActivityCommunication1.apk
      |InterComponentCommunication/ActivityCommunication2.apk
      |InterComponentCommunication/ActivityCommunication3.apk
      |InterComponentCommunication/ActivityCommunication4.apk
      |InterComponentCommunication/ActivityCommunication5.apk
      |InterComponentCommunication/ActivityCommunication6.apk
      |InterComponentCommunication/ActivityCommunication7.apk
      |InterComponentCommunication/ActivityCommunication8.apk
      |InterComponentCommunication/BroadcastTaintAndLeak1.apk
      |InterComponentCommunication/ComponentNotInManifest1.apk
      |InterComponentCommunication/EventOrdering1.apk
      |InterComponentCommunication/IntentSink1.apk
      |InterComponentCommunication/IntentSink2.apk
      |InterComponentCommunication/IntentSource1.apk
      |InterComponentCommunication/ServiceCommunication1.apk
      |InterComponentCommunication/SharedPreferences1.apk
      |InterComponentCommunication/Singletons1.apk
      |InterComponentCommunication/UnresolvableIntent1.apk
      |Lifecycle/ActivityLifecycle1.apk
      |Lifecycle/ActivityLifecycle2.apk
      |Lifecycle/ActivityLifecycle3.apk
      |Lifecycle/ActivityLifecycle4.apk
      |Lifecycle/ActivitySavedState1.apk
      |Lifecycle/ApplicationLifecycle1.apk
      |Lifecycle/ApplicationLifecycle2.apk
      |Lifecycle/ApplicationLifecycle3.apk
      |Lifecycle/AsynchronousEventOrdering1.apk
      |Lifecycle/BroadcastReceiverLifecycle1.apk
      |Lifecycle/BroadcastReceiverLifecycle2.apk
      |Lifecycle/EventOrdering1.apk
      |Lifecycle/FragmentLifecycle1.apk
      |Lifecycle/FragmentLifecycle2.apk
      |Lifecycle/ServiceLifecycle1.apk
      |Lifecycle/ServiceLifecycle2.apk
      |Lifecycle/SharedPreferenceChanged1.apk
      |Reflection/Reflection1.apk
      |Reflection/Reflection2.apk
      |Reflection/Reflection3.apk
      |Reflection/Reflection4.apk
      |Threading/AsyncTask1.apk
      |Threading/Executor1.apk
      |Threading/JavaThread1.apk
      |Threading/JavaThread2.apk
      |Threading/Looper1.apk
    """.replace(".apk", "-dex2jar.jar").stripMargin.split("\\n").filter(_.nonEmpty)

  for (test1 <- tests) {
    test(test1) {
      WalaAnalysis.main(List(getClass.getResource("/"+test1).getPath).toArray)
    }
  }

}
