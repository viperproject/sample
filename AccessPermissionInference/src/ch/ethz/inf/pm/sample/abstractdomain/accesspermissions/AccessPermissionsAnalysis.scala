package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions
import ch.ethz.inf.pm.sample.abstractdomain._
import ch.ethz.inf.pm.sample.abstractdomain.heapanalysis._
import ch.ethz.inf.pm.sample.property._
import ch.ethz.inf.pm.sample.oorepresentation.NativeMethodSemantics

class AccessPermissionsAnalysis[I <: NonRelationalHeapIdentifier[I]] extends SemanticAnalysis[SymbolicPermissionsDomain[I]] {

  override def getNativeMethodsSemantics() : List[NativeMethodSemantics] = List(ChaliceNativeMethodSemantics)

  override def reset() = {
    ConstraintsInference.emptyConstraints;
    CollectedResults.constraints=Set.empty
    CollectedResults.r=Map.empty;
  };

  override def getLabel() = "Access permissions inference"

  override def parameters() : List[(String, Any)] =
    List((("DischargeEverythingOnPostconditions", false)), (("UnsoundInhaling", true)), (("UnsoundDischarging", true)), (("PriorityOfContracts", 3)), (("PriorityOfMonitorInvariants", 2)), (("PriorityOfAbstractPredicates", 1)), (("Permissions", List("Chalice", "Fractional", "Counting"))))

  override def setParameter(label : String, value : Any) : Unit = label match {
      //Workaroud, problems with priority
    case "PriorityOfContracts" => Settings.priorityContracts = value.asInstanceOf[Int]*value.asInstanceOf[Int]*value.asInstanceOf[Int];
    case "PriorityOfMonitorInvariants" => Settings.priorityInvariants = value.asInstanceOf[Int]*value.asInstanceOf[Int]*value.asInstanceOf[Int];
    case "PriorityOfAbstractPredicates" => Settings.priorityPredicates = value.asInstanceOf[Int]*value.asInstanceOf[Int]*value.asInstanceOf[Int];
    case "UnsoundInhaling" => Settings.unsoundInhaling = value.asInstanceOf[Boolean];
    case "UnsoundDischarging" => Settings.unsoundDischarging = value.asInstanceOf[Boolean];
    case "DischargeEverythingOnPostconditions" => Settings.dischargeEverythingOnPostcondition = value.asInstanceOf[Boolean];
    case "Permissions" => value match {
      case "Fractional" => Settings.permissionType = FractionalPermissions;
      case "Counting" => Settings.permissionType = CountingPermissions;
      case "Chalice" => Settings.permissionType = ChalicePermissions;
    }
  }

  override def getInitialState() : SymbolicPermissionsDomain[I] = new SymbolicPermissionsDomain[I]();

  override def getProperties : List[Property] = List(new InferenceProperty(),new ContractsProperty())
}