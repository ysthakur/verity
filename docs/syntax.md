# Syntax

The syntax for Verity, in pseudo-EBNF.

## Files

```ebnf
file = module*;
```

## Modules

```ebnf
module = module-stmt, module-member;
module-member = module | typedef | value-def;
```

## Value definition

Not a variable inside a function, just a value inside a module

```ebnf
value-def = norm-value-def | given-value-def | const-value-def;
norm-value-def = "let", identifier, (":", type)?, "=", expr;
# A given can have just the identifier or just the type or both
given-value-def = "given", ((identifier, (":", type)?) | type), "=", expr;
const-value-def = "const", identifier, (":", type)?, "=", expr;
```

## Typedefs

```ebnf
type-def = record | enum | type-alias;

record = "record", identifier, const-param-list, val-param-list*;

enum = "enum", identifier, const-param-list, enum-case, (",", enum-case)*;

type-alias = "type", identifier, const-param-list, "=", type;
```

## Parameter lists

```ebnf
param-list = val-param-list | const-param-list;

# Value/runtime parameters, not compiletime parameters
val-param-list = norm-param-list | given-param-list | with-param-list;
norm-param-list = "(", norm-params, ")"; # A normal parameter list with parentheses
given-param-list = "(", "given", norm-params, ")";
norm-params = (norm-param, ",")*, norm-param?;
norm-param = identifier, ":", type;

const-param-list = type-param-list | proof-param-list;
type-param-list = "[", (type-param, ",")*, type-param?, "]";
type-param = identifier; # todo put context bounds on them
proof-param-list = "{", (proof-param, ",")*, proof-param?, "}";
proof-param = identifier, ":", type;
```

## Expressions

```ebnf
expr = TODO;
```

## Type expressions

```ebnf
type = TODO;
```

## Identifiers

```ebnf
identifier = TODO;
```
