import java.util.{LinkedList => JList, List => _, _}

final class C[T] extends JList[T] {
	def x = 2 + 2

	def y = x

	def z = C.this.x
}
