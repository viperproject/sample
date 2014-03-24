import javax.servlet.ServletContext
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new ch.ethz.inf.pm.sample.web.SilApp, "/*")
  }
}
