package validation

case class Error(message: String, path: List[String] = Nil) {
  def prefixed(field: String): Error =
    this.copy(path = field :: this.path)
}
