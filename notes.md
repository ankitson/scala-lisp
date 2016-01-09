 * a "define symbol expr" is interpreted as adding a binding in the current scope
   vs. a "let ((symbol expr) ...) (body_expr)" which evaluates the body with the defined symbols in scope

 * fn args are strictly evaluated
  * so evaluating if-else is a special case in the interpreter :(
  * we could implement lazy, or lazy+memo eval for lambdas. but native methods have to be strict.

 * todo next - see failing test. scoping for nested closures is broken.
