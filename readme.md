# TBD

Simple functional programming language with modern syntax
and batteries included standard library.

## Description

### Primatives

#### Integer

```
x = 12;
y = x + 1;
```

#### Boolean

```
x = true;
y = false;
```

#### Char

```
x = 'a';
y = 'b';
```

#### String

```
s = "hello";
s = "hello " ^ "world";
```

#### Float

```
x = 12.0;
y = x +. 1;
```

#### Option

```
x = Some 12;
y = None;
```

#### Result

```
x = Ok 12;
y = Error "not good";
```

#### Sum types (enums)

```
type Status = Success | Failure
s = Status.Success

type Box 'a = List of 'a | Tuple of 'a
box = List [1, 2, 3];
box = Tuple (1, 'a', "hi");
```

#### Product types (records)

```
type Person = { name : string, age : int }
john = Person { name = "John", age = 30 };
```

#### Joining Types

```
type Collection =
    | Label
    | Container of int
    | Record { field : string }

a = Collection.Label;
b = Collection.Container 12;
c = Collection.Record { field = "hello" };
```

#### Lists (O(1) insertion)

```
a = [1, 2, 3, 4];
```

#### Arrays (O(1) access)

```
a = {1, 2, 3, 4};
```

#### Variables and Functions

```
# variable, called once at first reference
# val x : int
x = 1;

# function, called every time referenced
# val f : 'a -> 'a
f x = x;
```

#### Conditionals

```
x = 1;
y = 2;


z = match x > y {
    true -> x
    false -> y
};
```

#### Pattern matching

```
x = (1, 2, 3);

x' = match x {
    (x, y, z) -> (z, y, x)
};

l = [1, 2, 3];

l' = match l {
    [] -> ...
    [e1, e2, e3] -> ...
    hd :: tl -> ...
};

# val rotate : int * int * int -> int * int * int
rotate (x, y, z) = (y, z, x)

x = Some 12;
y = match x {
    Some n -> n
    None -> 0
}
```
