package ChaliceExamples
import Chalice._

class Test() {
	def main() {
		val obj : Any = new String();
		Chalice.share(obj);
	}
}
