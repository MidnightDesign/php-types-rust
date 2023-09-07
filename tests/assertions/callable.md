- `callable(): string` is a subtype of `callable(): string`
- `callable(): string` is a subtype of `callable(): void`

- `callable(): void` is a subtype of `callable(): void`

- `callable(string): float` is a subtype of `callable(string): float`

- `callable(string): int` is a subtype of `callable(string): float`
- `callable(string): int` is a subtype of `callable(string): int`
- `callable(string): int` is a subtype of `callable(string, bool): int`
- `callable(string): int` is a subtype of `callable(string, int): int`

- `callable(string=): int` is a subtype of `callable(string): float`
- `callable(string=): int` is a subtype of `callable(string): int`
- `callable(string=): int` is a subtype of `callable(string=): int`
- `callable(string=): int` is a subtype of `callable(string, bool): int`
- `callable(string=): int` is a subtype of `callable(string, int): int`

- `callable(string, bool): int` is a subtype of `callable(string, bool): int`

- `callable(string, int): int` is a subtype of `callable(string, int): int`

- `callable(string | int): int` is a subtype of `callable(string): float`
- `callable(string | int): int` is a subtype of `callable(string): int`
- `callable(string | int): int` is a subtype of `callable(string, bool): int`
- `callable(string | int): int` is a subtype of `callable(string, int): int`
- `callable(string | int): int` is a subtype of `callable(string | int): int`
