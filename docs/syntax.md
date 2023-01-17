# Syntax

The syntax for Verity, in pseudo-EBNF.

## Files

```ebnf
file = module*;
```

## Modules

```ebnf
module = module-stmt, module-member, "end"?;
module-member = module | typedef | value-def;
```

## Value definition

Not a variable inside a function, just a value inside a module.

TODO name these something better

```ebnf
value-def = norm-value-def | given-value-def | const-value-def;
norm-value-def = "let", lower-id, (":", type)?, "=", expr;
# A given can have just the identifier or just the type or both
given-value-def = "given", ((lower-id, (":", type)?) | type), "=", expr;
# todo decide if consts should be uppercase instead
const-value-def = "const", lower-id, (":", type)?, "=", expr;
```

## Typedefs

```ebnf
type-def = record | enum | type-alias;

record = "record", upper-id, const-param-list*, val-param-list*;

enum = "enum", upper-id, const-param-list*, val-param-list*, enum-case, (",", enum-case)*;
enum-case = upper-id, const-param-list*, val-param-list+, ("->", const-arg-list*, val-arg-list*)?;

type-alias = "type", upper-id, const-param-list*, "=", type;
```

## Parameter lists

```ebnf
param-list = val-param-list | const-param-list;

# Value/runtime parameters, not compiletime parameters
val-param-list = norm-param-list | given-param-list | with-param-list;
norm-param-list = "(", norm-params, ")"; # A normal parameter list with parentheses
given-param-list = "(", "given", norm-params, ")";
norm-params = (norm-param, ",")*, norm-param?;
norm-param = lower-id, ":", type;

const-param-list = type-param-list | proof-param-list;
type-param-list = "[", (type-param, ",")*, type-param?, "]";
type-param = upper-id; # todo put context bounds on them
proof-param-list = "{", (proof-param, ",")*, proof-param?, "}";
proof-param = lower-id, ":", type;
```

## Expressions

```ebnf
expr = lambda | let-expr | if-expr | assignment;

lambda = "\", const-param-list*, val-param-list*, "->", expr;
let-expr = ("let", lower-id, (":", type), "=" expr)+, "in", expr;
if-expr = "if", expr, "then", expr, "else", expr;
assignment = binop, ("=" expr)?;

binop = see below;

fn-call-and-prop-access = selectable, (prop-access | const-arg-list | val-arg-list)*;
prop-access = ".", lower-id;
val-arg-list = "(", ")";

# Something that can be used as the left part in `foo.bar`, i.e. something with
# higher precedence than `.`
selectable = paren-expr | var-ref | literal;
paren-expr = "(", expr, ")";
var-ref = lower-id;
literal = num-literal | char-literal | string-literal;
```

### Binary operators

This is what binary operators look like, not going to bother defining them all.

```ebnf
binop = binop-1;
binop-1 = binop-2, (op-1, binop-2)*;
binop-2 = binop-3, (op-2, binop-3)*;
...
binop-x = fn-call, (op-x, fn-call);
```

The precedence of an operator is determined by its first character.
Precedence, from lowest to highest:

```text
|
^
&
=, !
<, >
+, -
*, /, %
?, ~, @, \, $
```

An operator is a sequence of one or more of the characters above. One caveat is
that if an operator ends with `=`, it must also start with `=`. Otherwise, it is
interpreted to be an augmented assignment, e.g. `+=`.

## Type expressions

```ebnf
type = upper-id, (type-member-access | const-arg-list)*;
type-member-access = ".", upper-id;
const-arg-list = type-arg-list | proof-arg-list;
type-arg-list = "[", (type, ",")*, type?, "]";
proof-arg-list = "{", (expr, ",")*, expr?, "}";
```

## Identifiers

```ebnf
lower-id = lowercase identifier;
upper-id = uppercase identifier;
```
