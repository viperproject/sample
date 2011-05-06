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
    List((("Unsound inhaling", true)), (("Unsound discharging", true)), (("Priority of contracts", 1)), (("Priority of monitor invariants", 2)), (("Priority of abstract predicates", 3)), (("Type of permissions", List("Chalice", "Fractional", "Counting"))))

  override def setParameter(label : String, value : Any) : Unit = label match {
    case "Priority of contracts" => Settings.priorityContracts = value.asInstanceOf[Int];
    case "Priority of monitor invariants" => Settings.priorityInvariants = value.asInstanceOf[Int];
    case "Priority of abstract predicates" => Settings.priorityPredicates = value.asInstanceOf[Int];
    case "Unsound inhaling" => Settings.unsoundInhaling = value.asInstanceOf[Boolean];
    case "Unsound discharging" => Settings.unsoundDischarging = value.asInstanceOf[Boolean];
    case "Type of permissions" => value match {
      case "Fractional" => Settings.permissionType = FractionalPermissions;
      case "Counting" => Settings.permissionType = CountingPermissions;
      case "Chalice" => Settings.permissionType = ChalicePermissions;
    }
  }

  override def getInitialState() : SymbolicPermissionsDomain[I] = new SymbolicPermissionsDomain[I]();

  override def getProperties() : Set[Property] = Set.empty+new InferenceProperty();
}