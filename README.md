# snock

[Nock][spec] interpreter in Scheme.

## Usage

### [Chez][chez]

```
$ scheme --script snock.ss
((39 40) 2 (0 3) 1 (2 (4 0 1) 1 (4 0 1)))
42
```

### [Guile][guile]

```
$ guile -s snock.ss
((39 40) 2 (0 3) 1 (2 (4 0 1) 1 (4 0 1)))
42
```

## See also

* [Urbit][urbit]
* [Urbit documentation][urbitdocs]
* [Nock documentation][nockdocs]

[spec]: https://github.com/urbit/urbit/blob/master/Spec/nock/5.txt
[urbit]: https://urbit.org/
[urbitdocs]: https://urbit.org/docs/
[nockdocs]: https://urbit.org/docs/nock
[chez]: http://www.scheme.com/
[guile]: http://www.gnu.org/software/guile/
