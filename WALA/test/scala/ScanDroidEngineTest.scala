import ch.ethz.inf.pm.sample.wala.ScanDroidEngine
import org.scalatest.FunSuite

/**
 * @author Lucas Brutschy
 */
class ScanDroidEngineTest extends FunSuite {

  test("Should run on the first examples") {
    ScanDroidEngine.makeFromJar(getClass.getResource("/Aliasing/Merge1.apk").getPath)
  }

}
