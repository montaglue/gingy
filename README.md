
![logo should be here](https://github.com/montaglue/gingy/blob/master/icon.png?raw=true)

# Gingy
A strongly typed programming language with an expressive type system and garbage collector.
I'm creating it to explore the balance between rust and go.
And also to combine a functional paradigm with control over memory allocation.

For the syntax, I used Rust and Haskell as a basis. Taking the main part from Rust, I decided to simplify the work with functions based on Haskell.

## Goals

- Explore LLVM opportunities to optimize programming language
- Find the time bottlenecks in compilation processes
- Use one of cutting edge real-time garbage collection algorithms

## Ideas

- Keep things low level, while providing access to pure functional language beauty
- Keep fast code execution and code writing processes
- Make language that tent's you to write safe code


## Examples

```rust
main = {
    println("Hello world")
}
```


```rust
not_curry_by_default : (a: u32, b: u32) -> u32 = {
    a + b
}

you_can_do_it_manualy = std.functional.curry(not_curry_by_default)


main = {
    println("2 + 2 = %d", you_can_do_it_manualy(2)(2))
}
```

```rust
main = {
    print("semicolon ")
    print("is ")
    print("social")
    println("construct")
}
```

## Progress

- [x] parser
- [ ] type checker
- [ ] internal representation generation
- [ ] visualization of compiling process
