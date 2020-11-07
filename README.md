# Erlang wrapper for unix libcrypt library

crypt-erl is a safe port wrapper for [crypt(3)](https://manpages.debian.org/crypt(3)) and [crypt_gensalt(3)](https://manpages.debian.org/crypt(3)) routines.

## Basic usage example

Generating random salt for Blowfish (bcrypt) method and using it to hash "Password":

```erlang
1> {ok, Salt} = crypt:gensalt("$2a$").
{ok,"$2a$05$gwtgJ3kUiZSjqwVs91/wXe"}
2> {ok, Hash} = crypt:crypt("Password", Salt).
{ok,"$2a$05$gwtgJ3kUiZSjqwVs91/wXePGURhb0vMWeMHQKimYflWAic4o7qEHm"}
```

Verifying password for given hash:
```erlang
1> Verify = fun(Phrase, Hash) -> {ok, Hash} =:= crypt:crypt(Phrase, Hash) end.
#Fun<erl_eval.13.126501267>
2> Verify("Password", "$2a$05$gwtgJ3kUiZSjqwVs91/wXePGURhb0vMWeMHQKimYflWAic4o7qEHm").
true
3> Verify("WrongPassword", "$2a$05$gwtgJ3kUiZSjqwVs91/wXePGURhb0vMWeMHQKimYflWAic4o7qEHm").
false
4> Verify("UnixSha512Pass", "$6$qtweESu3ihqnZJsN$SzhUFnMe7Diz7HkO5kirxkb01ubZaxDfR4sQMMPNLea4oDfJNa5Wv4rz6QscrESyZZMBXEtnBFmPUD5eiU0NZ.").
true
```

## Exports

### `crypt:gensalt`

```erlang
crypt:gensalt(Prefix) -> {ok, Salt} | {error, Reasor};
crypt:gensalt(Prefix, Rounds) -> {ok, Salt} | {error, Reason}.
```

Generates salt for given by `Prefix` crypt method with either default for given method number of rounds or for specified number of `Rounds`.

Valid prefixes (see also [crypt](https://en.wikipedia.org/wiki/Crypt_(C)#Key_derivation_functions_supported_by_crypt) on wikipedia):

| Prefix  | Method |
|---------|--------|
| `""`    | DES    |
| `"_"`   | BSDi   |    
| `"$1$"` | MD5    |
| `$2a$"`, `$2b$"`, `$2y$` | brypt (Blowfish) |
| `"$3$"` | NTHASH |
| `"$5$"` | SHA-256 |
| `"$6$"` | SHA-512 |
| `"$md5$"` | Solaris MD5 |
| `"$sha1$"` | PBKDF1 with SHA-1 |

Please note that not all methods are available on all systems.

### `crypt:crypt`

```erlang
crypt:crypt(Phrase, Salt) -> {ok, Hash} | {error, Reason}.
```

Return a hashed password `Phrase` for given `Salt`. 

### `crypt:change_workers_limits`

```erlang
crypt:change_workers_limits(MinWorkers, MaxWorkers) -> ok.
```

The default crypt application configuration is `{crypt, [{min_workers, 0}, {max_workers, 16}]}.`.

`MaxWorkers` controls maximum number of concurrently working port instances. 
If all workers are busy further requests will be queued for later execution.

`MinWorkers` controls minimum number of port instances which are kept alive 
awaiting for requests for immediate execution. 

## License

This library is licensed under the MIT license.