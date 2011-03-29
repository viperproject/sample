package ch.ethz.inf.pm.sample.abstractdomain.accesspermissions
import ch.ethz.inf.pm.sample.abstractdomain._

class AccessPermissionsAnalysis extends Analysis {
  override def getLabel() = "Access permissions inference"

  override def parameters() : Set[(String, Boolean)] =
    Set.empty.+(("unsoundInhaling", false))+(("unsoundDischarging", false))+(("priorityContracts", true))+(("priorityInvariants", true))+(("priorityPredicates", true))+(("fractionalPermissions", false))+(("countingPermissions", false))+(("ChalicePermissions", false))

  override def setIntegerParameter(label : String, value : Int) : Unit = label match {
    case "priorityContracts" => Settings.priorityContracts = value;
    case "priorityInvariants" => Settings.priorityInvariants = value;
    case "priorityPredicates" => Settings.priorityPredicates = value;
  }

  override def setBooleanParameter(label : String, value : Boolean) : Unit = label match {
    case "unsoundInhaling" => Settings.unsoundInhaling = value;
    case "unsoundDischarging" => Settings.unsoundDischarging = value;
    case "fractionalPermissions" =>
      if(Settings.permissionType!=null)
        throw new PermissionsException("Only one system of permissions is allowed by a single analysis")
      else Settings.permissionType = FractionalPermissions;
    case "countingPermissions" =>
      if(Settings.permissionType!=null)
        throw new PermissionsException("Only one system of permissions is allowed by a single analysis")
      else Settings.permissionType = CountingPermissions;
    case "ChalicePermissions" =>
      if(Settings.permissionType!=null)
        throw new PermissionsException("Only one system of permissions is allowed by a single analysis")
      else Settings.permissionType = ChalicePermissions;
  }

  override def getInitialState[S <: SemanticDomain[S]]() : S = new SymbolicPermissionsDomain().asInstanceOf[S];
}