# notes

type scheme:
  scheme 'a . 'b means that type 'b
  exists for all instances of type 'a.
  for example, 'a . 'a -> () is the
  type for the 'ignore' function.

generalize:
  ???

instantiate:
  instantiating a type scheme creates
  a new type from that scheme when
  given "parameter types" for that
  scheme. for example, instantiating
  scheme 'a . 'a -> int with type
  bool would create bool -> int

# expressions

## grouping

env ⊢ '(' e ')' : t ⊣ C
  if env ⊢ e : t ⊣ C

## binding

env ⊢ x '=' e1 '=>' e2 : t2 ⊣ {C1, C2}
  if env ⊢ e1 : t1 ⊣ C1
  and generalize(C1, env, x : t1) ⊢ e2 : t2 ⊣ C2

## lambda

env ⊢ '\' x '=>' e : 't1 -> t2 ⊣ C
  if fresh 't1
  and {env, x : 't1} ⊢ e : t2 ⊣ C

## sequence

env ⊢ e1 ';' e2 : t ⊣ C
  if env ⊢ e2 : t ⊣ C

## if-expressions

env ⊢ e1 '?' e2 ':' e3 : 't ⊣ {C1, C2, C3, t1 = bool, 't = t2, 't = t3}
  if fresh 't
  and env ⊢ e1 : t1 ⊣ C1
  and env ⊢ e2 : t2 ⊣ C2
  and env ⊢ e3 : t3 ⊣ C3

## application

env ⊢ e1 e2 : 't ⊣ {C1, C2, t1 = t2 -> 't}
  if fresh 't
  and env ⊢ e1 : t1 ⊣ C1
  and env ⊢ e2 : t2 ⊣ C2

## tuple

env ⊢ e1 ';' e2 : t1; t2 ⊣ {C1, C2}
  if env ⊢ e1 : t1 ⊣ C1
  and env ⊢ e2 : t2 ⊣ C2

## construct

depends on definition

## literal

env ⊢ b : Bool ⊣ {}
env ⊢ i : Int ⊣ {}
env ⊢ f : Float ⊣ {}
env ⊢ c : Char ⊣ {}
env ⊢ s : Str ⊣ {}
env ⊢ n : Unit ⊣ {}

## identifier

env ⊢ n : instantiate(env(n)) ⊣ {}
