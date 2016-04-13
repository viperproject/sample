/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

import javax.servlet.ServletContext
import org.scalatra.LifeCycle

class ScalatraBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context.mount(new ch.ethz.inf.pm.sample.web.SilApp, "/sil/*")
    context.mount(new ch.ethz.inf.pm.sample.web.TouchDevelopApp, "/td/*")
  }
}
