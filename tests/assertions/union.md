- `'foo' | 'bar'` is a subtype of `'foo' | 'bar'`
- `'foo' | 'bar'` is a subtype of `int | string`
- `'foo' | 'bar'` is a subtype of `list<string> | string`
- `'foo' | 'bar'` is a subtype of `non-empty-string`
- `'foo' | 'bar'` is a subtype of `scalar`
- `'foo' | 'bar'` is a subtype of `string`
- `'foo' | 'bar'` is a subtype of `string | 'foo'`
- `'foo' | 'bar'` is a subtype of `string | int`
- `'foo' | 'bar'` is a subtype of `string | int | bool`
- `'foo' | 'bar'` is a subtype of `string | list<string>`

- `bool | true` is a subtype of `bool`
- `bool | true` is a subtype of `bool | true`
- `bool | true` is a subtype of `false | true`
- `bool | true` is a subtype of `scalar`
- `bool | true` is a subtype of `string | int | bool`
- `bool | true` is a subtype of `true | false`

- `false | list<string>` is a subtype of `false | list<string>`

- `false | true` is a subtype of `bool`
- `false | true` is a subtype of `bool | true`
- `false | true` is a subtype of `false | true`
- `false | true` is a subtype of `scalar`
- `false | true` is a subtype of `string | int | bool`
- `false | true` is a subtype of `true | false`

- `int | string` is a subtype of `int | string`
- `int | string` is a subtype of `scalar`
- `int | string` is a subtype of `string | int`
- `int | string` is a subtype of `string | int | bool`

- `list<string> | list<int>` is a subtype of `array`
- `list<string> | list<int>` is a subtype of `array<array-key, mixed>`
- `list<string> | list<int>` is a subtype of `array<array-key, string | int>`
- `list<string> | list<int>` is a subtype of `array<mixed>`
- `list<string> | list<int>` is a subtype of `array{}`
- `list<string> | list<int>` is a subtype of `list<int | string>`
- `list<string> | list<int>` is a subtype of `list<string> | list<int>`
- `list<string> | list<int>` is a subtype of `iterable`
- `list<string> | list<int>` is a subtype of `iterable<mixed>`
- `list<string> | list<int>` is a subtype of `iterable<mixed, mixed>`

- `list<string> | string` is a subtype of `list<string> | string`
- `list<string> | string` is a subtype of `string | list<string>`

- `string | 'foo'` is a subtype of `int | string`
- `string | 'foo'` is a subtype of `list<string> | string`
- `string | 'foo'` is a subtype of `scalar`
- `string | 'foo'` is a subtype of `string`
- `string | 'foo'` is a subtype of `string | 'foo'`
- `string | 'foo'` is a subtype of `string | int`
- `string | 'foo'` is a subtype of `string | int | bool`
- `string | 'foo'` is a subtype of `string | list<string>`

- `string | int` is a subtype of `int | string`
- `string | int` is a subtype of `scalar`
- `string | int` is a subtype of `string | int`
- `string | int` is a subtype of `string | int | bool`

- `string | int | bool` is a subtype of `scalar`
- `string | int | bool` is a subtype of `string | int | bool`

- `string | list<string>` is a subtype of `list<string> | string`
- `string | list<string>` is a subtype of `string | list<string>`

- `true | false` is a subtype of `bool`
- `true | false` is a subtype of `bool | true`
- `true | false` is a subtype of `false | true`
- `true | false` is a subtype of `scalar`
- `true | false` is a subtype of `string | int | bool`
- `true | false` is a subtype of `true | false`
